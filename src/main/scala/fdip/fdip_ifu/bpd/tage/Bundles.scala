package boom.fdip.ifu.bpd.tage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import boom.fdip.common._
import boom.fdip.ifu._
import boom.fdip.ifu.bpd._
import boom.fdip.ifu.bpd.l1btb.BoomL1BTBParams

class TageEntry(params: BoomTageParams = BoomTageParams())(implicit p: Parameters)
  extends TageBundle()(p) {
  val valid = Bool()
  val tag   = UInt(params.tagBits.W)
  val ctr   = UInt(3.W)
}

class TageTableReadReq(params: BoomTageParams = BoomTageParams())(implicit p: Parameters)
  extends TageBundle()(p) {
  val setIdx = UInt(log2Ceil(params.tableInfo.head._1).W)
}

class TageTableReadResp(params: BoomTageParams = BoomTageParams())(implicit p: Parameters)
  extends TageBundle()(p) {
  val valid = Bool()
  val ctr = UInt(3.W)
  val tag = UInt(params.tagBits.W)
  val u   = UInt(2.W)
}

class TageTableWriteReq(params: BoomTageParams = BoomTageParams())(implicit p: Parameters)
  extends TageBundle()(p) {
  val setIdx      = UInt(log2Ceil(params.tableInfo.head._1).W)
  val writeEntry  = Bool()
  val writeUseful = Bool()
  val entry       = new TageEntry(params)
  val u           = UInt(2.W)
}

class TagePredictMeta(val nTables: Int)(implicit p: Parameters) extends TageBundle()(p) {
  val provider     = Vec(predWidth, Valid(UInt(log2Ceil(nTables).W)))
  val alt_differs  = Vec(predWidth, Bool())
  val provider_u   = Vec(predWidth, UInt(2.W))
  val provider_ctr = Vec(predWidth, UInt(3.W))
  val allocate     = Vec(predWidth, Valid(UInt((nTables).W)))
}

class TagePrediction(implicit p: Parameters) extends TageBundle()(p) {
  val taken = Vec(l1btbParameters.nWays, Valid(Bool()))
}

class TageFromBtbInfo(implicit p: Parameters) extends TageBundle()(p) {
  val results     = Vec(l1btbParameters.nWays, Valid(new Prediction()))
  val s1_position = Vec(l1btbParameters.nWays, UInt(log2Ceil(fetchBytes).W))
}

class TagePredictorIO(implicit p: Parameters) extends TageBundle()(p) {
  val s0_valid  = Input(Bool())
  val s0_pc     = Input(UInt(vaddrBitsExtended.W))
  val s0_ghist  = Input(UInt(globalHistoryLength.W))
  val tage_pred = Output(new TagePrediction())
  val fromBtb   = Input(new TageFromBtbInfo())
  val meta      = Output(new TagePredictMeta(tageParameters.tableInfo.size))
  val train     = Input(Valid(new BpuTrain))
}
