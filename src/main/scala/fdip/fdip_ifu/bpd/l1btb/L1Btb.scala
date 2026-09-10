package boom.fdip.ifu.bpd.l1btb

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.fdip.common._
import boom.fdip.util.{BoomCoreStringPrefix}
import boom.fdip.ifu._
import boom.fdip.ifu.bpd._
import boom.perf._

import scala.math.min

/*
s0: request stage
s1: read stage
s2: response stage
 */

class L1BTB(implicit p: Parameters) extends L1BtbModule()(p)
with L1BTBHelper{
	val params = l1BtbParams

	val io = IO(new Bundle {
		val f0_valid = Input(Bool())
		val f0_pc    = Input(UInt(vaddrBitsExtended.W))

		val resp    = Output(Vec(params.nWays, Valid(new Prediction)))
		// Store the meta as a UInt, use width inference to figure out the shape
		val meta = Output(Vec(params.nWays,
		Valid(new boom.fdip.ifu.bpd.l1btb.BTBPredictMeta())))

		val s1_position = Output(Vec(params.nWays, UInt(log2Ceil(fetchBytes).W)))

		val train = Input(Valid(new Train))
	})
	override val nSets         = params.nSets
	override val nWays         = params.nWays
	val tagSz         = vaddrBitsExtended - log2Ceil(nSets) - log2Ceil(fetchBytes) - 1
	val offsetSz      = params.offsetSz


	require(isPow2(nSets))

	val doing_reset = RegInit(true.B)
	val reset_idx   = RegInit(0.U(log2Ceil(nSets).W))
	reset_idx := reset_idx + doing_reset
	when (reset_idx === (nSets-1).U) { doing_reset := false.B }


	val btb      = Seq.fill(nWays) {SyncReadMem(nSets, new BTBEntry(offsetSz, tagSz))}
	val counters = Seq.fill(nWays) {SyncReadMem(nSets, UInt(2.W))}

	def inc_ctr(ctr: UInt, taken: Bool): UInt = {
		Mux(!taken, Mux(ctr === 0.U, 0.U, ctr - 1.U),
					Mux(ctr === 3.U, 3.U, ctr + 1.U))
	}
	val s0_valid = io.f0_valid && !doing_reset
	val s0_pc    = io.f0_pc
	val s0_idx   = fetchIdx(s0_pc)
	val s1_pc 	= RegNext(s0_pc)
	val s1_idx 	= RegNext(s0_idx)
	val s2_pc 	= RegNext(s1_pc)

  val s1_req_rbtb  = WireInit(VecInit(btb.map {entry => entry.read(s0_idx , s0_valid) }))
  val s1_req_rcnt = WireInit(VecInit(counters.map {entry => entry.read(s0_idx , s0_valid) }))
  val s1_req_tag   = WireInit(s1_idx >> log2Ceil(nSets))



	val s1_hits = VecInit((0 until nWays) map { w =>
		s1_req_rbtb(w).tag === s1_req_tag(tagSz-1,0)
	})
	io.s1_position := s1_req_rbtb.map(_.position)
	val s1_valid    = RegNext(s0_valid)
	val s1_hit_way  = PriorityEncoder(s1_hits)


	val s1_instOffset = s1_pc(log2Ceil(fetchBytes)-1,1)


	val s2_valid    	= RegNext(s1_valid)
	val s2_hits 			= RegNext(s1_hits)
	val s2_req_rbtb   = RegNext(s1_req_rbtb)
	val s2_req_rcnt   = RegNext(s1_req_rcnt)

	val s2_instOffset = RegNext(s1_instOffset)


	for (w <- 0 until nWays) {
		val entry = s2_req_rbtb(w) 
		io.resp(w).valid := !doing_reset && s2_hits(w) && s2_valid && entry.position >= s2_instOffset
		io.resp(w).bits.cfiPosition := entry.position
		io.resp(w).bits.attribute := entry.attribute
		io.resp(w).bits.taken := !entry.attribute.isNone && s2_req_rcnt(w)(1) === 1.U //direct's ctr(1) is 1
		io.resp(w).bits.target := getFullTarget(s2_pc, entry.target, Some(entry.targetCarry))

		io.meta(w).valid := !doing_reset && s2_hits(w) && s2_valid 
		io.meta(w).bits.position := entry.position
		io.meta(w).bits.attribute := entry.attribute
		io.meta(w).bits.hit := s2_hits(w)    
		io.meta(w).bits.ctr := s2_req_rcnt(w)
	}




	/* 
	t0: request stage
	t1: read stage
	t2: update stage
	but for now btb is dual ported, so there is no need for t1 stage, t0 and t2 are enough
	 */
	val t0_valid = io.train.valid
	val t0_pc    = io.train.bits.startPc
	val t0_idx  = fetchIdx(t0_pc)
	val t0_tag  = t0_idx>> log2Ceil(nSets)
	val t0_meta = io.train.bits.btbMeta
	val t0_cfi = io.train.bits.cfi

	val t1_tag = RegNext(t0_tag)
	val t1_idx = RegNext(t0_idx)
	val t1_meta = RegNext(t0_meta)
	val t1_cfi = RegNext(t0_cfi)
	val t1_valid = RegNext(t0_valid)
	val t1_pc = RegNext(t0_pc)
	// Read the trained block in parallel with the registered training metadata.
	// The metadata identifies positions; the memory read preserves each entry's
	// target and current saturating counter for per-way updates.
	// val t1_entries = VecInit(btb.map(_.read(t0_idx, t0_valid)))
	val t1_blockHits = t1_meta.map(meta => meta.valid && meta.bits.hit)
	val t1_cfiHits = t1_blockHits.zip(t1_meta).map { case (hit, meta) =>
		hit && meta.bits.position === t1_cfi.position && t1_cfi.valid
	}
	val t1_hasConditional = t1_blockHits.zip(t1_meta).map { case (hit, meta) =>
		hit && meta.bits.attribute.isConditional
	}.reduce(_ || _)
	val t1_cfiHit = t1_cfiHits.reduce(_ || _)
	val t1_priorHits = t1_blockHits.zip(t1_meta).map { case (hit, meta) =>
		hit && meta.bits.position < t1_cfi.position
	}
	val t1_cfiHitEntry = Mux1H(t1_cfiHits, t1_meta.map(_.bits))

	def incCtr(ctr: UInt, taken: Bool): UInt = {
		Mux(taken,
			Mux(ctr === 3.U, 3.U, ctr + 1.U),
			Mux(ctr === 0.U, 0.U, ctr - 1.U))
	}

	// On a CFI miss, prefer an unused way or an entry at/after the CFI. This
	// keeps all earlier branches available for the not-taken updates below.
	val replacementCandidates = VecInit(t1_blockHits.zip(t1_meta).map { case (hit, meta) =>
		!hit || meta.bits.position >= t1_cfi.position
	})
	val replacementExists = replacementCandidates.reduce(_ || _)
	val randomWay = LFSR(nWays, t1_valid && t1_cfi.valid && !t1_cfiHit)(log2Ceil(nWays) - 1, 0)
	val replacementWay = Mux(replacementExists,
		PriorityEncoder(replacementCandidates), randomWay)
	val replacementMask = UIntToOH(replacementWay, nWays)

	val t1_newEntry = Wire(new BTBEntry(offsetSz, tagSz))
	t1_newEntry.position := t1_cfi.position
	t1_newEntry.attribute := t1_cfi.attribute
	t1_newEntry.target := t1_cfi.target(offsetSz + instOffsetBits - 1, instOffsetBits)
	t1_newEntry.targetCarry := getTargetCarry(t1_pc, t1_cfi.target)
	t1_newEntry.tag := t1_tag(tagSz - 1, 0)

	val t1_trainCfi = t1_valid && t1_cfi.valid
	//TODO: add indirect branch update logic
	val t1_needUpdateEntry = t1_valid && t1_cfi.valid && t1_cfi.mispredict && (!t1_cfiHit || t1_cfiHitEntry.attribute =/= t1_cfi.attribute || t1_cfi.attribute.isOtherIndirect)
	val t1_updateEntryWay = Mux(!t1_cfiHit, replacementWay, PriorityEncoder(t1_cfiHits))

	val t1_needUpdateCtr = WireInit(VecInit(Seq.tabulate(nWays)(_ => false.B)))
	val t1_updateCtr = Wire(Vec(nWays,UInt(2.W)))
	val t1_entryWrite = VecInit((0 until nWays).map { w =>
		t1_needUpdateEntry && t1_updateEntryWay === w.U
	})

	for (w <- 0 until nWays) {
		t1_needUpdateCtr(w) := t1_valid && (t1_cfiHits(w) || t1_priorHits(w)) && t1_meta(w).bits.attribute.isConditional ||
			t1_needUpdateEntry && t1_updateEntryWay === w.U
	    t1_updateCtr(w) := MuxCase(0.U,
			Seq(
				(t1_needUpdateEntry && t1_updateEntryWay === w.U) -> Mux(t1_cfi.attribute.isConditional,Mux(t1_cfi.taken, 2.U, 1.U), 3.U),
				(t1_needUpdateCtr(w) && t1_cfiHits(w) && t1_cfi.attribute.isConditional) -> incCtr(t1_meta(w).bits.ctr, t1_cfi.taken),
				(t1_needUpdateCtr(w) && t1_priorHits(w)) -> incCtr(t1_meta(w).bits.ctr, false.B)
			)
		)

		when (t1_needUpdateEntry && t1_updateEntryWay === w.U) {
			btb(w).write(t1_idx, t1_newEntry)
		}
		when( t1_needUpdateCtr(w)) {
			counters(w).write(t1_idx, t1_updateCtr(w))
		}
	}

	XSPerfAccumulate("l1btb_train", t1_valid)
	XSPerfAccumulate("l1btb_train_cfi", t1_trainCfi)
	// Break down every resolved CFI training event by its architectural type.
	// Categories intentionally are not mutually exclusive: calls/returns are
	// also indirect (and a direct call is also direct), matching BranchAttribute
	// predicates used throughout the predictor.
	XSPerfSeqAccumulate("l1btb_train_cfi_type", t1_trainCfi, Seq(
		("total", true.B),
		("direct", t1_cfi.attribute.isDirect),
		("otherIndirect", t1_cfi.attribute.isOtherIndirect),
		("call", t1_cfi.attribute.isCall),
		("return", t1_cfi.attribute.isReturn),
		("conditional", t1_cfi.attribute.isConditional)
	))
	XSPerfAccumulate("l1btb_cfi_hit", t1_trainCfi && t1_cfiHit)
	XSPerfAccumulate("l1btb_cfi_miss", t1_trainCfi && !t1_cfiHit)
	XSPerfAccumulate("l1btb_entry_update", PopCount(t1_entryWrite))
	XSPerfAccumulate("l1btb_ctr_update", PopCount(t1_needUpdateCtr))
	XSPerfAccumulate("l1btb_entry_allocate", PopCount(VecInit((0 until nWays).map { w =>
		t1_trainCfi && !t1_cfiHit && replacementMask(w)
	})))
	XSPerfAccumulate("l1btb_entry_repair", PopCount(VecInit((0 until nWays).map { w =>
		t1_entryWrite(w) && t1_cfiHit
	})))
	XSPerfAccumulate("l1btb_ctr_cfi", PopCount(VecInit((0 until nWays).map { w =>
		t1_needUpdateCtr(w) && t1_cfiHits(w)
	})))
	XSPerfAccumulate("l1btb_ctr_prior", PopCount(VecInit((0 until nWays).map { w =>
		t1_needUpdateCtr(w) && t1_priorHits(w)
	})))
	XSPerfAccumulate("l1btb_prior_hit", PopCount(t1_priorHits))
	XSPerfAccumulate("l1btb_block_hit", PopCount(t1_blockHits))
}
