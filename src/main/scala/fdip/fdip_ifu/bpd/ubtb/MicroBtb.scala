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

import scala.math.min

class FAMicroBTBBranchPredictor(implicit p: Parameters) extends UbtbModule()(p)
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

  class MicroBTBEntry extends Bundle {
    val offset   = SInt(offsetSz.W)
    val attribute = new BranchAttribute
    val position = UInt(log2Ceil(fetchBytes).W)
    val tag   = UInt(tagSz.W)
    val ctr   = UInt(2.W)
  }


  val btb_entries      = Reg(Vec(nEntries,new MicroBTBEntry))

  val s1_req_tag   = s1_idx


  val s1_hit_ohs = btb_entries map { entry =>
    entry.tag === s1_req_tag(tagSz-1,0)
  }
  val s1_hits      = s1_hit_ohs.reduce(_||_)
  val s1_hit_entry = PriorityEncoder(s1_hit_ohs)

  io.resp.valid       := s1_hits && s1_valid
  io.resp.bits.attribute     := Mux(s1_hits, btb_entries(s1_hit_entry).attribute, 0.U.asTypeOf(new BranchAttribute))
  io.resp.bits.cfiPosition   := Mux(s1_hits, btb_entries(s1_hit_entry).position, 0.U)
  io.resp.bits.target        := Mux(s1_hits, (s1_pc.asSInt + btb_entries(s1_hit_entry).offset).asUInt, 0.U)
  io.resp.bits.taken         := s1_hits


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
  val t1_posMismatch = t1_valid && t1_hit && (btb_entries(t1_hit_entry).position =/= t1_position)
  val t1_attrMismatch = t1_valid && t1_hit && (btb_entries(t1_hit_entry).attribute.asUInt =/= t1_attribute.asUInt)
  
  t0_hitT1Victim := t1_valid && (t0_hit_entry === t1_update_way) && t1_allocate
  t0_hitT1Update := t1_valid && (t0_tag === t1_tag) && (t1_allocate || t1_hit)

  t1_update_entry := t1_existing_entry

  when(t1_valid && !t1_hit){
    t1_update_entry.offset := (t1_target.asSInt - t1_pc.asSInt).asSInt
    t1_update_entry.attribute := t1_attribute
    t1_update_entry.position := t1_position
    t1_update_entry.tag := t1_tag(tagSz-1,0)
    t1_update_entry.ctr := 2.U
  }

  when(t1_valid && t1_hit){
    when(!t1_taken){
      when(btb_entries(t1_hit_entry).ctr === 0.U || t1_posMismatch || t1_attrMismatch){
        t1_update_entry.offset := (t1_target.asSInt - t1_pc.asSInt).asSInt
        t1_update_entry.attribute := t1_attribute
        t1_update_entry.position := t1_position
        t1_update_entry.tag := t1_tag(tagSz-1,0)
        t1_update_entry.ctr := 2.U
      }.otherwise{
        t1_update_entry.ctr := btb_entries(t1_hit_entry).ctr - 1.U
      }
    }.otherwise{
        t1_update_entry.ctr := Mux(btb_entries(t1_hit_entry).ctr === 3.U, 3.U, btb_entries(t1_hit_entry).ctr + 1.U)
    }
  }
  when(t1_valid){
    when(!t1_hit){
      t1_update_entry.offset := (t1_target.asSInt - t1_pc.asSInt).asSInt
      t1_update_entry.attribute := t1_attribute
      t1_update_entry.position := t1_position
      t1_update_entry.tag := t1_tag(tagSz-1,0)
      t1_update_entry.ctr := 2.U
    }.elsewhen(!t1_taken || t1_posMismatch || t1_attrMismatch){
      when(btb_entries(t1_hit_entry).ctr === 0.U){
        t1_update_entry.offset := (t1_target.asSInt - t1_pc.asSInt).asSInt
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
}
