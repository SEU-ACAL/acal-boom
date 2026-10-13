package boom.fdip.ifu.bpd.l1btb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.fdip.common._
import boom.fdip.util.{BoomCoreStringPrefix}
import boom.fdip.ifu._
import boom.fdip.ifu.bpd._
import boom.perf._

import scala.math.min

class L1BtbTrainTrace(implicit p: Parameters) extends L1BtbBundle()(p) {
  val startPc = UInt(vaddrBitsExtended.W)
  val way = UInt(log2Ceil(l1btbParameters.nWays max 2).W)
  val cfiValid = Bool()
  val cfiPosition = UInt(log2Ceil(fetchBytes).W)
  val actualTaken = Bool()
  val mispredict = Bool()
  val hit = Bool()
  val cfiHit = Bool()
  val priorHit = Bool()
  val needUpdateCtr = Bool()
  val updateEntry = Bool()
  val ctrBefore = UInt(2.W)
  val ctrAfter = UInt(2.W)
}

/*
s0: request stage
s1: read stage
s2: response stage
 */

class L1BTB(implicit p: Parameters) extends L1BtbModule()(p)
with L1BTBHelper{
	val params = l1BtbParams
	private val trainTrace = ChiselDB.createTable("L1BtbTrainTrace", new L1BtbTrainTrace)

	val io = IO(new Bundle {
		val f0_valid = Input(Bool())
		val f0_pc    = Input(UInt(vaddrBitsExtended.W))

		val resp    = Output(Vec(params.nWays, Valid(new Prediction)))
		// Store the meta as a UInt, use width inference to figure out the shape
		val meta = Output(Vec(params.nWays,
		Valid(new boom.fdip.ifu.bpd.l1btb.BTBPredictMeta())))

		val s1_position = Output(Vec(params.nWays, UInt(log2Ceil(fetchBytes).W)))
		val s1_weak = Output(Vec(params.nWays, Bool()))

		val train = Input(Valid(new BpuTrain))
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
	// Validity is kept in resettable metadata registers. The BTB entry SRAMs
	// themselves are not synchronously cleared, so a tag read is never exposed
	// until its corresponding valid bit has been set by an entry write.
	val entryValid = RegInit(VecInit(Seq.fill(nWays)(VecInit(Seq.fill(nSets)(false.B)))))
	val replacer = Module(new L1BtbReplacer)
	val entryWriteBypass = RegInit(0.U.asTypeOf(new BTBEntryWriteBypass(offsetSz, tagSz)))
	when (doing_reset) {
		for (w <- 0 until nWays) {
			entryValid(w)(reset_idx) := false.B
		}
	}

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
	val s1_req_valid = RegNext(VecInit(entryValid.map(way => way(s0_idx(log2Ceil(nSets)-1, 0)))))
	val s1_req_tag   = WireInit(s1_idx >> log2Ceil(nSets))



	val s1_hits = VecInit((0 until nWays) map { w =>
		s1_req_valid(w) && s1_req_rbtb(w).tag === s1_req_tag(tagSz-1,0)
	})
	io.s1_position := s1_req_rbtb.map(_.position)
	io.s1_weak := VecInit((0 until nWays) map { w =>
		s1_req_valid(w) && (s1_req_rcnt(w)=== 1.U || s1_req_rcnt(w)=== 2.U)
	})
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
	val t0_valid = io.train.valid && !doing_reset
	val t0_pc    = io.train.bits.startPc
	val t0_idx  = fetchIdx(t0_pc)
	val t0_tag  = t0_idx>> log2Ceil(nSets)
	val t0_cfi = io.train.bits.cfi

	val t1_tag = RegNext(t0_tag)
	val t1_idx = RegNext(t0_idx)
	val t1_cfi = RegNext(t0_cfi)
	val t1_valid = RegNext(t0_valid)
	val t1_pc = RegNext(t0_pc)
	val t1_instOffset = t1_pc(log2Ceil(fetchBytes)-1, 1)
	// Read the current training block from the BTB SRAMs. FTQ metadata is a
	// prediction snapshot and can name a way that has since been replaced, so
	// replacement and counter training use the current tag/position instead.
	val t1_req_entries = VecInit(btb.map(_.read(t0_idx, t0_valid)))
	val t1_req_counters = VecInit(counters.map(_.read(t0_idx, t0_valid)))
	val t1_req_valid = RegNext(VecInit(entryValid.map(way => way(t0_idx(log2Ceil(nSets)-1, 0)))))
	val t1_entry_bypass_hit = VecInit((0 until nWays).map { w =>
		entryWriteBypass.valid &&
			entryWriteBypass.idx === t1_idx(log2Ceil(nSets)-1, 0) &&
			entryWriteBypass.way === w.U
	})
	val t1_current_entries = VecInit((0 until nWays).map { w =>
		Mux(t1_entry_bypass_hit(w), entryWriteBypass.entry, t1_req_entries(w))
	})
	val t1_current_valid = VecInit((0 until nWays).map { w =>
		Mux(t1_entry_bypass_hit(w), true.B, t1_req_valid(w))
	})
	val t1_blockHits = VecInit((0 until nWays).map { w =>
		t1_current_valid(w) && t1_current_entries(w).tag === t1_tag(tagSz-1, 0)
	})
	val t1_cfiHits = VecInit((0 until nWays).map { w =>
		t1_blockHits(w) && t1_current_entries(w).position >= t1_instOffset &&
			t1_current_entries(w).position === t1_cfi.position && t1_cfi.valid
	})
	val t1_cfiHit = t1_cfiHits.asUInt.orR
	val t1_priorHits = VecInit((0 until nWays).map { w =>
		// Entries before this request's start PC were not executed.
		t1_blockHits(w) && t1_current_entries(w).position >= t1_instOffset &&
			(!t0_cfi.valid || t1_current_entries(w).position < t1_cfi.position)
	})
	val t1_cfiHitEntry = Mux1H(t1_cfiHits, t1_current_entries)

	val t1_invalidMask = VecInit(t1_current_valid.map(valid => !valid)).asUInt
	val t1_protectedMask = VecInit((0 until nWays).map { w =>
		t1_blockHits(w) && t1_current_entries(w).position < t1_cfi.position && t1_cfi.valid
	}).asUInt
	val t1_eligibleMask = (~t1_protectedMask)(nWays - 1, 0)
	val t1_allWaysMask = ((BigInt(1) << nWays) - 1).U(nWays.W)
	val t1_candidateMask = Mux(
		t1_invalidMask.orR,
		t1_invalidMask,
		Mux(t1_eligibleMask.orR, t1_eligibleMask, t1_allWaysMask)
	)

	replacer.io.t0SetIdx := t0_idx(log2Ceil(nSets)-1, 0)
	replacer.io.t0Fire := t0_valid
	replacer.io.t1SetIdx := t1_idx(log2Ceil(nSets)-1, 0)
	replacer.io.t1CandidateMask := t1_candidateMask

	def incCtr(ctr: UInt, taken: Bool): UInt = {
		Mux(taken,
			Mux(ctr === 3.U, 3.U, ctr + 1.U),
			Mux(ctr === 0.U, 0.U, ctr - 1.U))
	}

	// The replacer performs invalid-way priority followed by masked LRU. The
	// mask is formed from current SRAM contents, rather than stale FTQ metadata.
	val replacementWay = replacer.io.t1Victim
	val replacementMask = UIntToOH(replacementWay, nWays)

	val t1_newEntry = Wire(new BTBEntry(offsetSz, tagSz))
	t1_newEntry.position := t1_cfi.position
	t1_newEntry.attribute := t1_cfi.attribute
	t1_newEntry.target := t1_cfi.target(offsetSz + instOffsetBits - 1, instOffsetBits)
	t1_newEntry.targetCarry := getTargetCarry(t1_pc, t1_cfi.target)
	t1_newEntry.tag := t1_tag(tagSz - 1, 0)

	val t1_trainCfi = t1_valid && t1_cfi.valid
	//TODO: add indirect branch update logic
	val t1_needUpdateEntry = t1_valid && t1_cfi.valid && t1_cfi.position >= t1_instOffset &&
		t1_cfi.mispredict && (!t1_cfiHit || t1_cfiHitEntry.attribute =/= t1_cfi.attribute || t1_cfi.attribute.needIttage)
	val t1_updateEntryWay = Mux(!t1_cfiHit, replacementWay, PriorityEncoder(t1_cfiHits))

	val t1_needUpdateCtr = WireInit(VecInit(Seq.tabulate(nWays)(_ => false.B)))
	val t1_updateCtr = Wire(Vec(nWays,UInt(2.W)))
	val t1_entryWrite = VecInit((0 until nWays).map { w =>
		t1_needUpdateEntry && t1_updateEntryWay === w.U
	})
	val t1_touchEntry = t1_entryWrite.asUInt.orR
	val t1_touchTakenCfi = t1_valid && t1_cfi.valid && t1_cfi.taken && t1_cfiHit
	replacer.io.t1TouchValid := t1_touchEntry || t1_touchTakenCfi
	replacer.io.t1TouchWay := Mux(
		t1_touchEntry,
		t1_updateEntryWay,
		PriorityEncoder(t1_cfiHits)
	)
	// FIFO of write events; entry 0 is newest and includes all concurrent ways.
	val ctrWriteBypass = RegInit(VecInit(Seq.fill(params.writeBypassEntries)(
		0.U.asTypeOf(new BTBWriteBypass))))
	val t1_baseCtr = VecInit((0 until nWays).map { w =>
		val bypassHits = VecInit(ctrWriteBypass.map { entry =>
			entry.valid(w) && entry.idx === t1_idx(log2Ceil(nSets)-1, 0) &&
				entry.tag === t1_tag(tagSz-1, 0) &&
				entry.position(w) === t1_current_entries(w).position
		})
		val bypassCtr = Mux1H(PriorityEncoderOH(bypassHits.asUInt), ctrWriteBypass.map(_.ctr(w)))
		Mux(bypassHits.asUInt.orR, bypassCtr, t1_req_counters(w))
	})
	dontTouch(t1_updateCtr)
	for (w <- 0 until nWays) {
		t1_needUpdateCtr(w) := t1_valid && (t1_cfiHits(w) || t1_priorHits(w)) && t1_current_entries(w).attribute.isConditional ||
			t1_needUpdateEntry && t1_updateEntryWay === w.U
	    t1_updateCtr(w) := MuxCase(0.U,
			Seq(
				(t1_needUpdateEntry && t1_updateEntryWay === w.U) -> Mux(t1_cfi.attribute.isConditional,Mux(t1_cfi.taken, 2.U, 1.U), 3.U),
				(t1_needUpdateCtr(w) && t1_cfiHits(w) && t1_cfi.attribute.isConditional) -> incCtr(t1_baseCtr(w), t1_cfi.taken),
				(t1_needUpdateCtr(w) && t1_priorHits(w)) -> incCtr(t1_baseCtr(w), false.B)
			)
		)

		when (t1_needUpdateEntry && t1_updateEntryWay === w.U) {
			btb(w).write(t1_idx, t1_newEntry)
			entryValid(w)(t1_idx(log2Ceil(nSets)-1, 0)) := true.B
		}
		when( t1_needUpdateCtr(w)) {
			counters(w).write(t1_idx, t1_updateCtr(w))
		}

		val trainTraceRecord = Wire(new L1BtbTrainTrace)
		trainTraceRecord.startPc := t1_pc
		trainTraceRecord.way := w.U
		trainTraceRecord.cfiValid := t1_cfi.valid
		trainTraceRecord.cfiPosition := t1_cfi.position
		trainTraceRecord.actualTaken := t1_cfi.taken
		trainTraceRecord.mispredict := t1_cfi.mispredict
		trainTraceRecord.hit := t1_blockHits(w)
		trainTraceRecord.cfiHit := t1_cfiHits(w)
		trainTraceRecord.priorHit := t1_priorHits(w)
		trainTraceRecord.needUpdateCtr := t1_needUpdateCtr(w)
		trainTraceRecord.updateEntry := t1_entryWrite(w)
		trainTraceRecord.ctrBefore := t1_baseCtr(w)
		trainTraceRecord.ctrAfter := Mux(t1_needUpdateCtr(w), t1_updateCtr(w), t1_baseCtr(w))
		trainTrace.log(trainTraceRecord, t1_valid && t1_cfi.valid, s"train-w${w}")(clock, reset)
	}

	// One entry write is possible per training event. Keep a one-cycle payload
	// bypass so a same-set allocation is visible to the next training read even
	// when the SRAM read-during-write mode is read-first.
	entryWriteBypass.valid := false.B
	when (t1_entryWrite.asUInt.orR) {
		entryWriteBypass.valid := true.B
		entryWriteBypass.idx := t1_idx(log2Ceil(nSets)-1, 0)
		entryWriteBypass.way := t1_updateEntryWay
		entryWriteBypass.entry := t1_newEntry
	}

	when (t1_needUpdateCtr.asUInt.orR) {
		for (entry <- 1 until params.writeBypassEntries) {
			ctrWriteBypass(entry) := ctrWriteBypass(entry - 1)
		}
		ctrWriteBypass(0).valid := t1_needUpdateCtr
		ctrWriteBypass(0).idx := t1_idx(log2Ceil(nSets)-1, 0)
		ctrWriteBypass(0).tag := t1_tag(tagSz-1, 0)
		ctrWriteBypass(0).position := VecInit((0 until nWays).map { w =>
			Mux(t1_entryWrite(w), t1_cfi.position, t1_current_entries(w).position)
		})
		ctrWriteBypass(0).ctr := t1_updateCtr
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
