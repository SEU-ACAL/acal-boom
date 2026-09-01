package boom.fdip.ifu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket.{TLBReq, TLBResp}
import boom.fdip.common._
import boom.fdip.util._

/*
 * S0 accepts one FTQ request. S1 holds that transaction until its ITLB/meta
 * result is written to WayLookup and the miss stage can accept it. ITLB and
 * Meta replays select the saved S1 address through request-side muxes.
 */
class PrefetchPipe(implicit p: Parameters) extends BoomModule()(p)
  with HasBoomCoreParameters
  with HasBoomFrontendParameters {

  val io = IO(new Bundle {
    val fromFtq = Flipped(DecoupledIO(new FTQToPrefetchIO))
    val flush   = Input(Bool())

    val metaRead = new MetaReadBundle()
    val tlbReq   = DecoupledIO(new TLBReq(log2Ceil(fetchBytes)))
    val tlbResp  = Input(new TLBResp(log2Ceil(fetchBytes)))
    val ptw      = Input(new PtwReadBundle())

    val wayLookupWrite = DecoupledIO(new WayLookupWrite)
    val missReq        = DecoupledIO(new ICacheMissReq())
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
  private val s1_pTag      = Reg(UInt(tagBits.W))
  private val s1_waymask   = Reg(UInt(nWays.W))
  private val s1_tlbDone   = RegInit(false.B)
  private val s1_metaDone  = RegInit(false.B)

  /* A fresh FTQ request drives both initial requests. s0_fire is the single
   * acceptance event for the S0-to-S1 transfer, after both endpoints accept. */
  s1_ready := !s1_valid && (s1_state === S1FsmState.Idle) && !io.flush
  private val s0_issue = io.fromFtq.valid && s1_ready
  io.fromFtq.ready := s1_ready && io.tlbReq.ready && io.metaRead.req.ready
  s0_fire := io.fromFtq.fire

  private val s1_itlbResend = s1_valid && (s1_state === S1FsmState.ItlbResend)
  io.tlbReq.valid := !io.flush && (s0_issue || s1_itlbResend)
  io.tlbReq.bits := DontCare
  io.tlbReq.bits.vaddr := Mux(s1_itlbResend, s1_vaddr, s0_vaddr)
  io.tlbReq.bits.passthrough := false.B
  io.tlbReq.bits.size := log2Ceil(coreInstBytes * fetchWidth).U
  io.tlbReq.bits.cmd := io.ptw.cmd
  io.tlbReq.bits.v := false.B
  io.tlbReq.bits.prv := io.ptw.prv

  // The FDIP ITLB response is combinational for the request in this cycle.
  // Capture the initial S0 response at s0_fire; a replay response is sampled
  // in the ItlbResend cycle itself.
  private val s0_tlbHit = s0_fire && !io.tlbResp.miss
  private val s1_tlbHit = s1_itlbResend && io.tlbReq.fire && !io.tlbResp.miss
  private val s1_tlbFinish = s1_tlbDone || s1_tlbHit

  private val s1_metaResend = s1_valid && (
    (s1_state === S1FsmState.ItlbResend && s1_tlbFinish) ||
      (s1_state === S1FsmState.MetaResend)
  )
  io.metaRead.req.valid := !io.flush && (s0_issue || s1_metaResend)
  io.metaRead.req.bits.vSetIdx := Mux(s1_metaResend, s1_vSetIdx, s0_vSetIdx)
  private val s1_metaRespValid = RegNext(io.metaRead.req.fire, false.B)
  private val s1_metaPTag = Mux(s1_tlbHit,
    io.tlbResp.paddr(tagBits + untagBits - 1, untagBits), s1_pTag)
  private val s1_metaHitVec = io.metaRead.resp.map { resp =>
    resp.valid && (resp.bits.pTag === s1_metaPTag)
  }
  private val s1_metaWaymask = Mux(s1_metaHitVec.reduce(_ || _), s1_metaHitVec.asUInt, 0.U)
  private val s1_metaFinishThisCycle = s1_metaRespValid && s1_tlbFinish
  //TODO: may have some  problems
  private val s1_metaFinish = s1_metaDone || s1_metaFinishThisCycle

  assert(PopCount(s1_metaHitVec) <= 1.U, "prefetch S1 metadata has multiple matching ways")

  when (io.flush) {
    s1_valid := false.B
    s1_tlbDone := false.B
    s1_metaDone := false.B
  }.elsewhen (s0_fire) {
    s1_valid := true.B
    s1_vaddr := s0_vaddr
    s1_vSetIdx := s0_vSetIdx
    s1_tlbDone := s0_tlbHit
    when (s0_tlbHit) {
      s1_pTag := io.tlbResp.paddr(tagBits + untagBits - 1, untagBits)
    }
    s1_metaDone := false.B
  }.otherwise {
    when (s1_tlbHit && s1_valid) {
      s1_tlbDone := true.B
      s1_pTag := io.tlbResp.paddr(tagBits + untagBits - 1, untagBits)
    }
    when (s1_metaFinishThisCycle && s1_valid) {
      s1_metaDone := true.B
      s1_waymask := s1_metaWaymask
    }
    when (s1_fire) {
      s1_valid := false.B
      s1_tlbDone := false.B
      s1_metaDone := false.B
    }
  }

  /* Once meta responds, retain its result for any later WayLookup backpressure. */
  private val s1_waymaskForEnq = Mux(s1_metaFinishThisCycle, s1_metaWaymask, s1_waymask)
  private val s1_pTagForEnq = Mux(s1_tlbHit,
    io.tlbResp.paddr(tagBits + untagBits - 1, untagBits), s1_pTag)
  private val s1_wayCanEnq = s1_valid && s1_tlbFinish && s1_metaFinish &&
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
  io.wayLookupWrite.bits.ftqIdx := 0.U


  private val s2_valid = RegInit(false.B)
  private val s2_tag = Reg(UInt(tagBits.W))
  private val s2_idx = Reg(UInt(nSetsBits.W))
  private val s2_hit = RegInit(false.B)


  when (io.flush) {
    s2_valid := false.B
  }.elsewhen (s1_fire) {
    s2_valid := true.B
    s2_tag := s1_pTagForEnq
    s2_idx := s1_vSetIdx
    s2_hit := s1_waymaskForEnq.orR
  }.elsewhen (s2_fire) {
    s2_valid := false.B
  }
  //TODO: consider the case when miss resp hit this
  private val s2_miss = s2_valid && !s2_hit
  io.missReq.valid := s2_miss && !io.flush
  io.missReq.bits.blkPaddr := Cat(s2_tag, s2_idx)
  io.missReq.bits.vSetIdx := s2_idx
  io.missReq.bits.way := if (isDM) 0.U else LFSR(16, s2_miss)(log2Ceil(nWays) - 1, 0)

  s2_fire  := s2_valid && (s2_hit || io.missReq.fire)
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
}
