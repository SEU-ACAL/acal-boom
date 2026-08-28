package boom.fdip.ifu.bpd.ubtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import boom.fdip.common._
import boom.fdip.ifu._
import boom.fdip.ifu.bpd._

class FAMicroBTBPredictMeta(implicit p: Parameters) extends UbtbBundle()(p) {
  val hits      = Vec(predWidth, Bool())
  val write_way = UInt(log2Ceil(UbtbEntries).W)
}

class FAMicroBTBPredictorIO(implicit p: Parameters) extends UbtbBundle()(p) {
  val f0_valid = Input(Bool())
  val f0_pc    = Input(UInt(vaddrBitsExtended.W))
  val resp     = Output(Valid(new Prediction()))
  val train    = Input(Valid(new FastTrain))
}

  class MicroBTBEntry (implicit p: Parameters) extends UbtbBundle()(p) {
    val target   = UInt(UbtbOffsetWidth.W)
    val attribute = new BranchAttribute
    val position = UInt(log2Ceil(fetchBytes).W)
    val tag   = UInt(UbtbTagWidth.W)
    val ctr   = UInt(2.W)
  }
