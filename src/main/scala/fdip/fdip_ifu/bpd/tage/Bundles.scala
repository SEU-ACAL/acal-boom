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

class TageTableReadReq(nRows: Int)(implicit p: Parameters)
  extends TageBundle()(p) {
  val setIdx = UInt(log2Ceil(nRows).W)
}

class TageTableReadResp(params: BoomTageParams = BoomTageParams())(implicit p: Parameters)
  extends TageBundle()(p) {
  val valid = Bool()
  val ctr = UInt(3.W)
  val tag = UInt(params.tagBits.W)
  val u   = UInt(2.W)
}

class TageTableWriteReq(nRows: Int, params: BoomTageParams = BoomTageParams())(implicit p: Parameters)
  extends TageBundle()(p) {
  val setIdx      = UInt(log2Ceil(nRows).W)
  val wayMask     = Vec(TageNways, Bool())
  val writeEntry  = Vec(TageNways, Bool())
  val writeUseful = Vec(TageNways, Bool())
  val entry       = Vec(TageNways, new TageEntry(params))
  val u           = Vec(TageNways, UInt(2.W))
}

class TageWriteBypass(implicit p: Parameters) extends TageBundle()(p) {
  // A table can write more than one way in one training cycle.  Keep the
  // bypass key/value per way so a write to way 1 cannot be mistaken for way 0.
  val valid = Vec(TageTables, Vec(TageNways, Bool()))
  val idx = MixedVec(tageParams.tableInfo.map { case (nRows, _, _) =>
    Vec(TageNways, UInt(log2Ceil(nRows).W))
  })
  val tag = Vec(TageTables, Vec(TageNways, UInt(TageTagWidth.W)))
  val ctr = Vec(TageTables, Vec(TageNways, UInt(3.W)))
  val uValid = Vec(TageTables, Vec(TageNways, Bool()))
  val u = Vec(TageTables, Vec(TageNways, UInt(2.W)))
}

class TagePredictMeta(val nTables: Int)(implicit p: Parameters) extends TageBundle()(p) {
  val provider     = Vec(predWidth, Valid(UInt(log2Ceil(nTables).W)))
  val useProvider  = Vec(predWidth, Bool())
  val altOrBasePred = Vec(predWidth, Bool())
  val alt_differs  = Vec(predWidth, Bool())
  val provider_u   = Vec(predWidth, UInt(2.W))
  val provider_ctr = Vec(predWidth, UInt(3.W))
  val provider_wayMask = Vec(predWidth, UInt(TageNways.W))
  val allocate     = Vec(predWidth, Valid(Vec(nTables,UInt(TageNways.W))))
  val altConf      = Vec(predWidth, Bool())
}

class TagePrediction(implicit p: Parameters) extends TageBundle()(p) {
  val taken = Vec(l1btbParameters.nWays, Valid(Bool()))
  val selectedEntry = Vec(l1btbParameters.nWays, new TageSelectedEntry())
}

class TageSelectedEntry(implicit p: Parameters) extends TageBundle()(p) {
  val valid = Bool()
  val tableIdx = UInt(log2Ceil(tageParameters.tableInfo.size max 2).W)
  val idx = UInt(log2Ceil(tageParameters.tableInfo.map(_._1).max).W)
  val lookupTag = UInt(tageParameters.tagBits.W)
  val storedTag = UInt(tageParameters.tagBits.W)
  val ctr = UInt(3.W)
  val ghist = UInt(globalHistoryLength.W)
}

class TageFromBtbInfo(implicit p: Parameters) extends TageBundle()(p) {
  val results     = Vec(l1btbParameters.nWays, Valid(new Prediction()))
  val s1_position = Vec(l1btbParameters.nWays, UInt(log2Ceil(fetchBytes).W))
  val s1_weak = Vec(l1btbParameters.nWays, Bool())
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
