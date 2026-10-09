package boom.fdip.ifu

import chisel3._
import chisel3.util._
import chisel3.util.random._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket.{TLBReq, TLBResp}
import boom.fdip.common._
import boom.fdip.util._
import boom.perf._

/*
 * S0 accepts one FTQ request. S1 holds that transaction until its ITLB/meta
 * result is written to WayLookup and the miss stage can accept it. ITLB and
 * Meta replays select the saved S1 address through request-side muxes.
 */
class PrefetchPipe(implicit p: Parameters) extends BoomModule()(p)
  with HasICacheParameters {

  val io = IO(new Bundle {
    val fromFtq = Flipped(DecoupledIO(new FTQToPrefetchIO))
    val flush   = Input(Bool())

    val metaRead = new MetaReadBundle()
    val tlbReq   = DecoupledIO(new TLBReq(log2Ceil(fetchBytes)))
    val tlbResp  = Input(new TLBResp(log2Ceil(fetchBytes)))
    val ptw      = Input(new PtwReadBundle())

    val wayLookupWrite = DecoupledIO(new WayLookupWrite)
    val missReq        = DecoupledIO(new ICacheMissReq())
    // Refill response used to bypass stale SRAM metadata while S1 is held.
    val missResp       = Flipped(ValidIO(new ICacheMissResp()))
  })

  private object S1FsmState extends ChiselEnum {
    val Idle, ItlbResend, MetaResend, EnqWay, EnterS2 = Value
  }

  private val s1_ready = WireDefault(false.B)
  private val s2_ready = WireDefault(false.B)
  private val s0_fire  = WireDefault(false.B)
  private val s1_fire  = WireDefault(false.B)
  private val s2_fire  = WireDefault(false.B)

  private val s0_vaddr   = io.fromFtq.bits.fetch_pc
  private val s0_vSetIdx = s0_vaddr(untagBits - 1, blockOffBits)

  private val s1_state     = RegInit(S1FsmState.Idle)
  private val s1_nextState = WireDefault(s1_state)
  private val s1_valid     = RegInit(false.B)
  private val s1_vaddr     = Reg(UInt(vaddrBitsExtended.W))
  private val s1_vSetIdx   = Reg(UInt(nSetsBits.W))
  private val s1_pTag      = RegInit(0.U(tagBits.W))
  private val s1_metaRegWaymask = RegInit(0.U(nWays.W))
  private val s1_metaRegValid = RegInit(false.B)
  private val s1_missRegValid = RegInit(false.B)
  private val s1_missRegTag = RegInit(0.U(tagBits.W))
  private val s1_missRegWaymask = RegInit(0.U(nWays.W))
  private val s1_tlbDone   = RegInit(false.B)
  private val s1_fetchFault = RegInit(false.B)
  private val s1_prefetchable = RegInit(false.B)
  private val s1_metaDone  = RegInit(false.B)

  /* A fresh FTQ request drives both initial requests. s0_fire is the single
   * acceptance event for the S0-to-S1 transfer, after both endpoints accept. */
  // A completing S1 transaction releases its slot in this cycle. Capture the
  // next S0 request at the same edge to sustain one lookup per cycle.
  s1_ready := !io.flush && (
    (!s1_valid && (s1_state === S1FsmState.Idle)) || s1_fire
  )
  private val s0_issue = io.fromFtq.valid && s1_ready
  io.fromFtq.ready := s1_ready && io.tlbReq.ready && io.metaRead.req.ready
  s0_fire := io.fromFtq.fire

  private val s1_itlbResend = s1_valid && (s1_state === S1FsmState.ItlbResend)
  io.tlbReq.valid := !io.flush && (s0_issue || s1_itlbResend)
  io.tlbReq.bits := DontCare
  io.tlbReq.bits.vaddr := Mux(s1_itlbResend, s1_vaddr, s0_vaddr)
  io.tlbReq.bits.passthrough := false.B
  io.tlbReq.bits.size := log2Ceil(coreInstBytes * fetchWidth).U
  io.tlbReq.bits.cmd := DontCare
  io.tlbReq.bits.v := io.ptw.v
  io.tlbReq.bits.prv := io.ptw.prv

  // The FDIP ITLB response is combinational for the request in this cycle.
  // Capture the initial S0 response at s0_fire; a replay response is sampled
  // in the ItlbResend cycle itself.
  private val s0_tlbHit = s0_fire && !io.tlbResp.miss
  private val s1_tlbHit = s1_itlbResend && io.tlbReq.fire && !io.tlbResp.miss
  private val s1_tlbFinish = s1_tlbDone || s1_tlbHit
  private val tlbFetchFault = io.tlbResp.ae.inst || io.tlbResp.pf.inst || io.tlbResp.gf.inst

  private val s1_metaNeedResend = s1_valid && (
    (s1_state === S1FsmState.ItlbResend && s1_tlbFinish) ||
      (s1_state === S1FsmState.MetaResend)
  )
  io.metaRead.req.valid := !io.flush && (s0_issue || s1_metaNeedResend)
  io.metaRead.req.bits.vSetIdx := Mux(s1_metaNeedResend, s1_vSetIdx, s0_vSetIdx)

  private val s1_metaRespValid = RegNext(io.metaRead.req.fire, false.B)
  // Only a replay refreshes the current S1 metadata. A new S0 read belongs to
  // the next transaction and must not hide the retiring S1's retained result.
  private val s1_metaReplayFire = s1_metaNeedResend && io.metaRead.req.fire

  private val s1_metaPTag = Mux(s1_tlbHit,
    io.tlbResp.paddr(tagBits + untagBits - 1, untagBits), s1_pTag)
  private val s1_metaHitVec = io.metaRead.resp.map { resp =>
    resp.valid && (resp.bits.pTag === s1_metaPTag)
  }
  // Source 2: metadata returned directly by the SRAM in this lookup.
  private val s1_sramWaymask = Mux(s1_metaHitVec.reduce(_ || _), VecInit(s1_metaHitVec).asUInt, 0.U)
  // Source 3: metadata retained while S1 waits for a replay or downstream
  // backpressure. The SRAM response is only a one-cycle pulse.
  private val s1_metaBaseWaymask = Mux(
    s1_metaRespValid,
    s1_sramWaymask,
    Mux(s1_metaRegValid && !s1_metaReplayFire, s1_metaRegWaymask, 0.U))
  // Source 1: refill/MISS response bypass. A refill of the same tag makes its
  // way valid; replacing the way with another tag invalidates the old hit.
  private val s1_missSetMatch = s1_valid && s1_tlbFinish && io.missResp.valid &&
    (io.missResp.bits.vSetIdx === s1_vSetIdx)
  private val s1_missTag = io.missResp.bits.blkPaddr >> idxBits
  private val s1_missWaymask = UIntToOH(io.missResp.bits.way, nWays)
  private val s1_pendingMissValid = s1_metaRespValid && s1_missRegValid && !s1_missSetMatch
  private val s1_metaMissValid = s1_missSetMatch || s1_pendingMissValid
  private val s1_metaMissTag = Mux(s1_missSetMatch, s1_missTag, s1_missRegTag)
  private val s1_metaMissWaymask = Mux(s1_missSetMatch, s1_missWaymask, s1_missRegWaymask)
  private val s1_metaWaymask = Mux(
    s1_metaMissValid,
    Mux(s1_metaMissTag === s1_metaPTag,
      s1_metaMissWaymask,
      Mux(s1_metaMissWaymask === s1_metaBaseWaymask, 0.U, s1_metaBaseWaymask)),
    s1_metaBaseWaymask)
  private val s1_metaFinishThisCycle = s1_metaRespValid && s1_tlbFinish
  private val s1_metaFinish = s1_metaDone || s1_metaFinishThisCycle

  assert(PopCount(s1_metaHitVec) <= 1.U, "prefetch S1 metadata has multiple matching ways")

  when (io.flush) {
    s1_valid := false.B
    s1_tlbDone := false.B
    s1_metaDone := false.B
    s1_metaRegValid := false.B
    s1_missRegValid := false.B
  }.elsewhen (s0_fire) {
    s1_valid := true.B
    s1_vaddr := s0_vaddr
    s1_vSetIdx := s0_vSetIdx
    s1_tlbDone := s0_tlbHit
    when (s0_tlbHit) {
      s1_pTag := io.tlbResp.paddr(tagBits + untagBits - 1, untagBits)
      s1_fetchFault := tlbFetchFault
      s1_prefetchable := io.tlbResp.prefetchable
    }
    s1_metaDone := false.B
    s1_metaRegValid := false.B
    s1_missRegValid := false.B
  }.otherwise {
    // A replay starts a fresh base value for this S1 request. A refill
    // arriving in the same/next cycle can then repopulate the register.
    when (s1_metaReplayFire) {
      s1_metaRegValid := false.B
    }
    when (s1_tlbHit && s1_valid) {
      s1_tlbDone := true.B
      s1_pTag := io.tlbResp.paddr(tagBits + untagBits - 1, untagBits)
      s1_fetchFault := tlbFetchFault
      s1_prefetchable := io.tlbResp.prefetchable
    }
    when (s1_metaFinishThisCycle && s1_valid) {
      s1_metaDone := true.B
      s1_metaRegWaymask := s1_metaWaymask
      s1_metaRegValid := true.B
    }
    when (s1_missSetMatch && s1_valid && !s1_metaFinishThisCycle) {
      s1_metaRegWaymask := s1_metaWaymask
      s1_metaRegValid := true.B
    }
    when (s1_missSetMatch && s1_valid) {
      s1_missRegValid := true.B
      s1_missRegTag := s1_missTag
      s1_missRegWaymask := s1_missWaymask
    }
    when (s1_fire) {
      s1_valid := false.B
      s1_tlbDone := false.B
      s1_metaDone := false.B
      s1_metaRegValid := false.B
      s1_missRegValid := false.B
    }
  }

  /* Once meta responds, retain its result for any later WayLookup backpressure. */
  private val s1_waymaskForEnq = Mux(s1_fetchFault, 0.U, s1_metaWaymask)
  private val s1_pTagForEnq = Mux(s1_tlbHit,
    io.tlbResp.paddr(tagBits + untagBits - 1, untagBits), s1_pTag)
  // TLB replay finishes in ItlbResend, where WayLookup cannot enqueue. In
  // Idle/EnqWay its result is already registered. Keep completion independent
  // of this cycle's TLB request so s1_fire can safely drive s1_ready.
  private val s1_wayCanEnq = s1_valid && s1_tlbDone &&
    (s1_metaDone || s1_metaRespValid) &&
    ((s1_state === S1FsmState.Idle) || (s1_state === S1FsmState.EnqWay))
  private val s1_wayFire = io.wayLookupWrite.fire

  s1_fire := s1_valid && !io.flush && (
    ((s1_state === S1FsmState.EnterS2) && s2_ready) ||
      (((s1_state === S1FsmState.Idle) || (s1_state === S1FsmState.EnqWay)) && s1_wayFire && s2_ready)
  )

  io.wayLookupWrite.valid := s1_wayCanEnq && !io.flush
  io.wayLookupWrite.bits.entry.vSetIdx := s1_vSetIdx
  io.wayLookupWrite.bits.entry.waymask := s1_waymaskForEnq
  io.wayLookupWrite.bits.entry.pTag := s1_pTagForEnq
  io.wayLookupWrite.bits.entry.fetchFault := s1_fetchFault
  io.wayLookupWrite.bits.ftqIdx := 0.U


  private val s2_valid = RegInit(false.B)
  private val s2_tag = Reg(UInt(tagBits.W))
  private val s2_idx = Reg(UInt(nSetsBits.W))
  private val s2_hit = RegInit(false.B)
  private val s2_skipPrefetch = RegInit(false.B)


  when (io.flush) {
    s2_valid := false.B
  }.elsewhen (s1_fire) {
    s2_valid := true.B
    s2_tag := s1_pTagForEnq
    s2_idx := s1_vSetIdx
    s2_hit := s1_waymaskForEnq.orR
    s2_skipPrefetch := s1_fetchFault || !s1_prefetchable
  }.elsewhen (s2_fire) {
    s2_valid := false.B
  }
  //TODO: consider the case when miss resp hit this
  private val s2_miss = s2_valid && !s2_hit && !s2_skipPrefetch
  io.missReq.valid := s2_miss && !io.flush
  io.missReq.bits.blkPaddr := Cat(s2_tag, s2_idx)
  io.missReq.bits.vSetIdx := s2_idx
  io.missReq.bits.way := (if (isDM) 0.U else LFSR(16, s2_miss)(log2Ceil(nWays) - 1, 0))

  s2_fire  := s2_valid && (s2_hit || s2_skipPrefetch || io.missReq.fire)
  s2_ready := !s2_valid || s2_fire

  switch (s1_state) {
    is (S1FsmState.Idle) {
      when (s1_valid) {
        when (!s1_tlbFinish) {
          s1_nextState := S1FsmState.ItlbResend
        }.elsewhen (!s1_metaFinish) {
          s1_nextState := S1FsmState.EnqWay
        }.elsewhen (!s1_wayFire) {
          s1_nextState := S1FsmState.EnqWay
        }.elsewhen (!s2_ready) {
          s1_nextState := S1FsmState.EnterS2
        }
      }
    }
    is (S1FsmState.ItlbResend) {
      when (s1_tlbFinish) {
        when (!io.metaRead.req.ready) {
          s1_nextState := S1FsmState.MetaResend
        }.otherwise {
          s1_nextState := S1FsmState.EnqWay
        }
      }
    }
    is (S1FsmState.MetaResend) {
      when (io.metaRead.req.fire) {
        s1_nextState := S1FsmState.EnqWay
      }
    }
    is (S1FsmState.EnqWay) {
      when (s1_wayFire) {
        when (!s2_ready) {
          s1_nextState := S1FsmState.EnterS2
        }.otherwise {
          s1_nextState := S1FsmState.Idle
        }
      }
    }
    is (S1FsmState.EnterS2) {
      when (s2_ready) {
        s1_nextState := S1FsmState.Idle
      }
    }
  }

  when (io.flush) {
    s1_nextState := S1FsmState.Idle
  }
  s1_state := s1_nextState

  // Separate accepted FTQ transactions from cycles waiting at the entrance.
  // An occupied S1 blocks S0 only when it cannot complete in this cycle.
  XSPerfSeqAccumulate("prefetch_s0", io.fromFtq.valid, Seq(
    ("fire", s0_fire),
    ("flush_blocked", io.flush),
    ("stall_s1", !io.flush && !s1_ready),
    ("stall_itlb", s1_ready && !io.tlbReq.ready),
    ("stall_meta", s1_ready && io.tlbReq.ready && !io.metaRead.req.ready)
  ))

  val perfS1WaitTlb = !s1_tlbFinish
  val perfS1WaitMeta = s1_tlbFinish && !s1_metaFinish
  val perfS1WaitWay = s1_tlbFinish && s1_metaFinish &&
    io.wayLookupWrite.valid && !io.wayLookupWrite.ready
  val perfS1WaitS2 = s1_tlbFinish && s1_metaFinish && !s2_ready &&
    ((s1_state === S1FsmState.EnterS2) || s1_wayFire)
  XSPerfSeqAccumulate("prefetch_s1", s1_valid && !io.flush, Seq(
    ("advance", s1_fire),
    ("wait_itlb", perfS1WaitTlb),
    ("wait_meta", perfS1WaitMeta),
    ("wait_waylookup", perfS1WaitWay),
    ("wait_s2", perfS1WaitS2),
    ("wait_control", !s1_fire && !perfS1WaitTlb && !perfS1WaitMeta &&
      !perfS1WaitWay && !perfS1WaitS2)
  ))
  XSPerfSeqAccumulate("prefetch_s2", s2_valid && !io.flush, Seq(
    ("hit", s2_hit),
    ("miss_valid", io.missReq.valid),
    ("miss_fire", io.missReq.fire),
    ("miss_stall", io.missReq.valid && !io.missReq.ready)
  ))
  XSPerfSeqAccumulate("prefetch_itlb", io.tlbReq.fire, Seq(
    ("req", true.B),
    ("miss", io.tlbResp.miss)
  ))
  XSPerfSeqAccumulate("prefetch_waylookup", io.wayLookupWrite.fire, Seq(
    ("hit", s1_waymaskForEnq.orR),
    ("miss", !s1_waymaskForEnq.orR)
  ))
  XSPerfSeqAccumulate("prefetch_flush", io.flush, Seq(
    ("s1", s1_valid),
    ("s2", s2_valid)
  ))
}
