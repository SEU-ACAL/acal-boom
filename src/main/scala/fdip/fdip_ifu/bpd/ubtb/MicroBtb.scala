package boom.fdip.ifu.bpd.ubtb

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.fdip.common._
import boom.fdip.util.{BoomCoreStringPrefix, WrapInc}
import boom.fdip.ifu._
import boom.fdip.ifu.bpd._
import boom.perf._

import scala.math.min

class FAMicroBTBBranchPredictor(implicit p: Parameters) extends UbtbModule()(p) with UBTBHelper
{
  val io = IO(new FAMicroBTBPredictorIO)

  val s0_idx       = fetchIdx(io.f0_pc)
  val s1_idx       = RegNext(s0_idx)
  val s2_idx       = RegNext(s1_idx)
  val s3_idx       = RegNext(s2_idx)
  val s0_valid     = io.f0_valid
  val s1_valid     = RegNext(s0_valid)
  val s2_valid     = RegNext(s1_valid)
  val s3_valid     = RegNext(s2_valid)
  val s0_pc        = io.f0_pc
  val s1_pc        = RegNext(s0_pc)


  val nEntries         = UbtbEntries
  val tagSz         = UbtbTagWidth
  val offsetSz      = UbtbOffsetWidth


  require(isPow2(nEntries), s"nEntries must be a power of 2, got $nEntries")




  val btb_entries      = Reg(Vec(nEntries,new MicroBTBEntry))

  val s1_req_tag   = s1_idx


  val s1_hit_ohs = btb_entries map { entry =>
    entry.tag === s1_req_tag(tagSz-1,0)
  }
  val s1_hits      = s1_hit_ohs.reduce(_||_)
  val s1_hit_entry = PriorityEncoder(s1_hit_ohs)
  val s1_instOffset = s1_pc(log2Ceil(fetchBytes)-1,1)
  val s1_hitAfterOffset = s1_hits && btb_entries(s1_hit_entry).position >= s1_instOffset
  io.resp.valid              := s1_valid && s1_hitAfterOffset
  io.resp.bits.attribute     := Mux(s1_hits, btb_entries(s1_hit_entry).attribute, 0.U.asTypeOf(new BranchAttribute))
  io.resp.bits.cfiPosition   := Mux(s1_hits, btb_entries(s1_hit_entry).position, 0.U)
  io.resp.bits.target        := getFullTarget(s1_pc, btb_entries(s1_hit_entry).target) 
  io.resp.bits.taken         := s1_hits && btb_entries(s1_hit_entry).position >= s1_instOffset


  val t0_valid = io.train.valid
  val t0_pc    = io.train.bits.startPc
  val t0_tag  = fetchIdx(t0_pc)
  val t0_attribute = io.train.bits.branch.attribute

  val t0_hitOH = btb_entries map { entry =>
    entry.tag === t0_tag(tagSz-1,0)
  }
  val t0_hitT1Victim = Wire(Bool())
  val t0_hitT1Update = Wire(Bool())
  val t0_hit = t0_hitOH.reduce(_||_) && !t0_hitT1Victim || t0_hitT1Update
  val t0_hit_entry = PriorityEncoder(t0_hitOH)


  val t1_hit = RegNext(t0_hit)
  val t1_hit_entry = RegNext(t0_hit_entry)
  val t1_valid = RegNext(t0_valid)
  val t1_tag  = RegNext(t0_tag)
  val t1_pc = RegNext(t0_pc)
  val t1_attribute = RegNext(t0_attribute)
  val t1_taken = RegNext(io.train.bits.branch.taken)
  val t1_position = RegNext(io.train.bits.branch.cfiPosition)
  val t1_target   = RegNext(io.train.bits.branch.target)
  val t1_allocate = t1_valid && !t1_hit && t1_taken
  val randomWay =
    if (nEntries == 1) {
      0.U
    } else {
      LFSR(16, t1_allocate)(log2Ceil(nEntries) - 1, 0)
    }
    
  val t1_update_entry = Wire(new MicroBTBEntry)
  val t1_existing_entry = Mux(t1_hit, btb_entries(t1_hit_entry), 0.U.asTypeOf(new MicroBTBEntry))
  val t1_update_way = Mux(t1_hit, t1_hit_entry, randomWay)
  val t1_posMismatch = WireInit(t1_valid && t1_hit && (btb_entries(t1_hit_entry).position =/= t1_position))
  val t1_attrMismatch = WireInit(t1_valid && t1_hit && (btb_entries(t1_hit_entry).attribute.asUInt =/= t1_attribute.asUInt))

  
  t0_hitT1Victim := t1_valid && (t0_hit_entry === t1_update_way) && t1_allocate
  t0_hitT1Update := t1_valid && (t0_tag === t1_tag) && (t1_allocate || t1_hit)

  dontTouch(t0_hitT1Victim)
  dontTouch(t0_hitT1Update)
  dontTouch(t1_posMismatch)
  dontTouch(t1_attrMismatch)
  t1_update_entry := t1_existing_entry


  when(t1_valid){
    when(!t1_hit){
      t1_update_entry.target := t1_target(offsetSz+instOffsetBits-1,instOffsetBits)
      t1_update_entry.attribute := t1_attribute
      t1_update_entry.position := t1_position
      t1_update_entry.tag := t1_tag(tagSz-1,0)
      t1_update_entry.ctr := 2.U
    }.elsewhen(!t1_taken || t1_posMismatch || t1_attrMismatch){
      when(btb_entries(t1_hit_entry).ctr === 0.U){
        t1_update_entry.target := t1_target(offsetSz+instOffsetBits-1,instOffsetBits)
        t1_update_entry.attribute := t1_attribute
        t1_update_entry.position := t1_position
        t1_update_entry.tag := t1_tag(tagSz-1,0)
        t1_update_entry.ctr := 2.U
      }.otherwise{
        t1_update_entry.ctr := btb_entries(t1_hit_entry).ctr - 1.U
      }
    }.otherwise{
      t1_update_entry.ctr := Mux(btb_entries(t1_hit_entry).ctr===3.U, 3.U, btb_entries(t1_hit_entry).ctr + 1.U)
    }
  }

  when(t1_valid&&(t1_allocate||t1_hit)){
    btb_entries(t1_update_way) := t1_update_entry
  }

  // UBTB is the f1 predictor. Keep lookup acceptance, offset filtering, and
  // each training outcome separate so prediction losses can be attributed
  // without changing its replacement or counter behavior.
  XSPerfAccumulate("ubtb_req", s0_valid)
  XSPerfAccumulate("ubtb_tag_hit", s1_valid && s1_hits)
  XSPerfAccumulate("ubtb_hit_before_offset", s1_valid && s1_hits && !s1_hitAfterOffset)
  XSPerfAccumulate("ubtb_resp_hit", io.resp.valid)
  XSPerfAccumulate("ubtb_train", t1_valid)
  XSPerfAccumulate("ubtb_train_taken", t1_valid && t1_taken)
  XSPerfAccumulate("ubtb_train_hit", t1_valid && t1_hit)
  XSPerfAccumulate("ubtb_allocate", t1_allocate)
  XSPerfAccumulate("ubtb_train_not_taken", t1_valid && t1_hit && !t1_taken)
  XSPerfAccumulate("ubtb_train_pos_mismatch", t1_posMismatch)
  XSPerfAccumulate("ubtb_train_attr_mismatch", t1_attrMismatch)
  XSPerfAccumulate("ubtb_train_rewrite", t1_valid && t1_hit &&
    (!t1_taken || t1_posMismatch || t1_attrMismatch) &&
    btb_entries(t1_hit_entry).ctr === 0.U)
}
