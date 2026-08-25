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

import scala.math.min

/*
s0: request stage
s1: read stage
s2: response stage
 */

class L1BTB(implicit p: Parameters) extends L1BtbModule()(p)
{
  val params = l1BtbParams

  val io = IO(new Bundle {
    val f0_valid = Input(Bool())
    val f0_pc    = Input(UInt(vaddrBitsExtended.W))

    val resp    = Output(Vec(params.nWays, Valid(new Prediction)))
    // Store the meta as a UInt, use width inference to figure out the shape
    val meta = Output(Vec(params.nWays,
      Valid(new boom.fdip.ifu.bpd.l1btb.BTBPredictMeta())))

	val s1_position = Output(Vec(params.nWays, UInt(log2Ceil(fetchBytes).W)))

    val train = Input(Valid(new BtbTrain))
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

	val s0_valid = io.f0_valid && !doing_reset
	val s0_pc    = io.f0_pc
	val s0_idx   = fetchIdx(s0_pc)
	val s1_pc 	= RegNext(s0_pc)
	val s1_idx 	= RegNext(s0_idx)
	val s2_pc 	= RegNext(s1_pc)

  val s1_req_rbtb  = WireInit(VecInit(btb.map {entry => entry.read(s0_idx , s0_valid) }))
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

	val s2_instOffset = RegNext(s1_instOffset)


	for (w <- 0 until nWays) {
		val entry = s2_req_rbtb(w) 
		io.resp(w).valid := !doing_reset && s2_hits(w) && s2_valid && entry.position >= s2_instOffset
		io.resp(w).bits.cfiPosition := entry.position
		io.resp(w).bits.attribute := entry.attribute
		io.resp(w).bits.taken := !entry.attribute.isNone && entry.ctr(1) === 1.U //direct's ctr(1) is 1
  		io.resp(w).bits.target := (s2_pc.asSInt + entry.offset).asUInt 

		io.meta(w).valid := !doing_reset && s2_hits(w) && s2_valid 
		io.meta(w).bits.position := entry.position
		io.meta(w).bits.attribute := entry.attribute
		io.meta(w).bits.hit := s2_hits(w)    
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
	val t0_meta = io.train.bits.meta
	val t0_cfi = io.train.bits.cfi

	val t1_tag = RegNext(t0_tag)
	val t1_idx = RegNext(t0_idx)
	val t1_meta = RegNext(t0_meta)
	val t1_cfi = RegNext(t0_cfi)
	val t1_valid = RegNext(t0_valid)
	val t1_pc = RegNext(t0_pc)
	val t1_hits = t1_meta.map{meta=>
		meta.valid && meta.bits.position === t1_cfi.position && meta.bits.hit
	}
	val t1_hitMask = WireInit(VecInit(PriorityEncoderOH(t1_hits)))
	val t1_hit = t1_hits.reduce(_||_)

	val t1_attributeMismatch = t1_hit &&
		t1_cfi.attribute.asUInt =/= Mux1H(t1_hitMask, t1_meta.map(_.bits.attribute.asUInt))
	val t1_entryNeedUpdate = t1_valid && (!t1_hit || t1_attributeMismatch ||
		t1_cfi.mispredict)
	val randomWay =LFSR(nWays, t1_entryNeedUpdate)(log2Ceil(nWays) - 1, 0)
	val t1_entry = Wire(new BTBEntry(offsetSz, tagSz))
		t1_entry.position := t1_cfi.position
		t1_entry.attribute := t1_cfi.attribute
		t1_entry.offset := (t1_cfi.target.asSInt - t1_pc.asSInt).asSInt
		t1_entry.tag := t1_tag(tagSz-1,0)
		t1_entry.ctr := Mux(t1_cfi.taken, 3.U, 0.U) //direct's ctr(1) is 1
		// Allocate on a miss and rewrite an existing entry only after a correction.
	val t1_wayMask = Mux(t1_hit,t1_hitMask.asUInt.asUInt,UIntToOH(randomWay,nWays).asUInt)
	for (w <- 0 until nWays) {
		when(t1_valid && t1_entryNeedUpdate && t1_wayMask(w).asBool){
      btb(w).write(t1_idx,t1_entry)
		}
	}

}
