package boom.fdip.ifu.bpd.l1btb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import boom.fdip.common._
import boom.fdip.ifu._
import boom.fdip.ifu.bpd._

class BTBEntry(val offsetSz: Int, val tagSz: Int)(implicit p: Parameters)
  extends L1BtbBundle()(p) {
  val attribute = new BranchAttribute
  val position  = UInt(log2Ceil(fetchBytes).W)
  val target    = UInt(offsetSz.W)
  val targetCarry = new TargetCarry
  val tag       = UInt(tagSz.W)
  // val ctr       = UInt(2.W)
}
class BTBResp(val offsetSz: Int, val tagSz: Int)(implicit p: Parameters)
  extends L1BtbBundle()(p) {
  val attribute = new BranchAttribute
  val position  = UInt(log2Ceil(fetchBytes).W)
  val target    = UInt(offsetSz.W)
  val targetCarry = new TargetCarry
  val tag       = UInt(tagSz.W)
  val ctr       = UInt(2.W)
}
class BTBPredictMeta(implicit p: Parameters) extends L1BtbBundle()(p) {
  val hit       = Bool()
  val position  = UInt(log2Ceil(fetchBytes).W)
  val attribute = new BranchAttribute
  val ctr       = UInt(2.W)
}

class BTBWriteBypass(implicit p: Parameters) extends L1BtbBundle()(p) {
  val valid = Vec(L1BtbWays, Bool())
  val idx = UInt(log2Ceil(L1BtbSets).W)
  val tag = UInt(L1BtbTagWidth.W)
  val position = Vec(L1BtbWays, UInt(log2Ceil(fetchBytes).W))
  val ctr = Vec(L1BtbWays, UInt(2.W))
}

class BTBEntryWriteBypass(val offsetSz: Int, val tagSz: Int)(implicit p: Parameters)
    extends L1BtbBundle()(p) {
  val valid = Bool()
  val idx = UInt(log2Ceil(L1BtbSets).W)
  val way = UInt(log2Ceil(L1BtbWays max 2).W)
  val entry = new BTBEntry(offsetSz, tagSz)
}
