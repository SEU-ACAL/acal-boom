package boom.fdip.ifu.bpd

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import boom.fdip.common._
import boom.fdip.util._
import boom.fdip.ifu._
import boom.fdip.ifu.bpd.l1btb.BoomL1BTBParams
import boom.fdip.ifu.bpd.ubtb.BoomFAMicroBTBParams
import boom.fdip.ifu.bpd.tage.BoomTageParams
import boom.fdip.ifu.bpd.tage.TagePredictMeta
import boom.fdip.ifu.bpd.l1btb.BTBPredictMeta
import boom.fdip.ifu.bpd.ras.RasState
class Prediction(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val cfiPosition = UInt(log2Ceil(fetchBytes).W)
  val target      = UInt(vaddrBitsExtended.W)
  val attribute   = new BranchAttribute
  val taken       = Bool()
}

class BranchInfo(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val target      = UInt(vaddrBitsExtended.W)
  val taken       = Bool()
  val cfiPosition = UInt(log2Ceil(fetchBytes).W)
  val attribute   = new BranchAttribute
  val mispredict  = Bool()
}

class ResolvedCfi(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val valid      = Bool()
  val position   = UInt(log2Ceil(predWidth).W)
  val taken      = Bool()
  val target     = UInt(vaddrBitsExtended.W)
  val attribute  = new BranchAttribute
  val mispredict = Bool()
}



class FastTrain(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val startPc = UInt(vaddrBitsExtended.W)
  val branch  = new BranchInfo
}

class Train(implicit p: Parameters) extends BoomBundle()(p)
  with HasBpdParameters
{
  val startPc  = UInt(vaddrBitsExtended.W)

  val phrMeta   = new PhrMeta
  val brMask  = UInt(predWidth.W)
  val cfi     = new ResolvedCfi
  val btbMeta = Vec(l1btbParameters.nWays,
    Valid(new boom.fdip.ifu.bpd.l1btb.BTBPredictMeta()))
  val tageMeta = new boom.fdip.ifu.bpd.tage.TagePredictMeta(BoomTageParams().tableInfo.size)
}

class BpuTrain(implicit p: Parameters) extends BoomBundle()(p)
  with HasBpdParameters
{
  val startPc  = UInt(vaddrBitsExtended.W)

  val phr   = UInt(globalHistoryLength.W)
  val brMask  = UInt(predWidth.W)
  val cfi     = new ResolvedCfi
  val btbMeta = Vec(l1btbParameters.nWays,
    Valid(new boom.fdip.ifu.bpd.l1btb.BTBPredictMeta()))
  val tageMeta = new boom.fdip.ifu.bpd.tage.TagePredictMeta(BoomTageParams().tableInfo.size)
}

class BranchPredictionMeta(implicit p: Parameters) extends BoomBundle()(p)
  with HasBpdParameters
{
  val tage = new TagePredictMeta(tageParameters.tableInfo.size)
  val btb  = Vec(l1btbParameters.nWays,Valid(new BTBPredictMeta()))
}

class BranchPredictionLiteBundle(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val pc          = UInt(vaddrBitsExtended.W)
  val valid       = Bool()
  val hasRedirect = Bool()
  val ftq_idx     = UInt((log2Ceil(ftqSz) + 1).W)
  val phrMeta       = new PhrMeta
  val pred        = new Prediction
  val meta        = Output(new BranchPredictionMeta)
  val rasState    = new RasState
  // val lhist       = Output(UInt(localHistoryLength.W))





}



class BranchPredictionRespBundle(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val f1 = new BranchPredictionLiteBundle
  val f2 = new BranchPredictionLiteBundle

  def selectedResp = PriorityMux(Seq(
    // (f3.valid && f3.hasRedirect) -> f3,
    (f2.valid && f2.hasRedirect) -> f2,
    f1.valid -> f1))

  def selectedRespIdxForFtq = PriorityMux(Seq(
    (f2.valid && f2.hasRedirect) -> BP_S2,
    f1.valid -> BP_S1))

  def selectedFtqentry = {
    val res = Wire(new FTQPCBundle)
  
    res.is_exit_branch := !selectedResp.pred.attribute.isNone
    res.is_exit_taken := selectedResp.pred.taken
    res.exit_pc_offset := selectedResp.pred.cfiPosition
    res.target := selectedResp.pred.target
    res.start_pc := selectedResp.pc
    res.mask := fetchMask(res.start_pc) & MaskLower(UIntToOH(res.exit_pc_offset))
    res
  }

  def lastStage = f2
}

class BPUToFTQIO(implicit p: Parameters) extends BoomBundle
{
  val resp = DecoupledIO(new BranchPredictionRespBundle)
}

class BranchPredictionUpdate(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val is_mispredict_update = Bool()
  val is_repair_update = Bool()
  val btb_mispredicts = UInt(predWidth.W)
  def is_btb_mispredict_update = btb_mispredicts =/= 0.U
  def is_commit_update = !(is_mispredict_update || is_repair_update || is_btb_mispredict_update)
  val pc = UInt(vaddrBitsExtended.W)
  val br_mask = UInt(predWidth.W)
  val cfi_idx = Valid(UInt(log2Ceil(predWidth).W))
  val cfi_taken = Bool()
  val cfi_mispredicted = Bool()
  val cfi_is_br = Bool()
  val cfi_is_jal = Bool()
  val cfi_is_jalr = Bool()
  val cfi_is_ret = Bool()
  val cfi_is_call = Bool()
  val ghist = new GlobalHistory
  val lhist = UInt(localHistoryLength.W)
  val target = UInt(vaddrBitsExtended.W)
  val meta = new BranchPredictionMeta
}

abstract class TypedBranchPredictorUpdate(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val is_mispredict_update = Bool()
  val is_repair_update = Bool()
  val btb_mispredicts = UInt(predWidth.W)
  def is_btb_mispredict_update = btb_mispredicts =/= 0.U
  def is_commit_update = !(is_mispredict_update || is_repair_update || is_btb_mispredict_update)
  val pc = UInt(vaddrBitsExtended.W)
  val br_mask = UInt(predWidth.W)
  val cfi_idx = Valid(UInt(log2Ceil(predWidth).W))
  val cfi_taken = Bool()
  val cfi_mispredicted = Bool()
  val cfi_is_br = Bool()
  val cfi_is_jal = Bool()
  val cfi_is_jalr = Bool()
  val cfi_is_call = Bool()
  val cfi_is_ret = Bool()
  val ghist = UInt(globalHistoryLength.W)
  val lhist = UInt(localHistoryLength.W)
  val target = UInt(vaddrBitsExtended.W)
}

class BranchPredictorUpdate(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val is_mispredict_update = Bool()
  val is_repair_update = Bool()
  val btb_mispredicts = UInt(predWidth.W)
  def is_btb_mispredict_update = btb_mispredicts =/= 0.U
  def is_commit_update = !(is_mispredict_update || is_repair_update || is_btb_mispredict_update)
  val pc = UInt(vaddrBitsExtended.W)
  val br_mask = UInt(predWidth.W)
  val cfi_idx = Valid(UInt(log2Ceil(predWidth).W))
  val cfi_taken = Bool()
  val cfi_mispredicted = Bool()
  val cfi_is_br = Bool()
  val cfi_is_jal = Bool()
  val cfi_is_jalr = Bool()
  val cfi_is_call = Bool()
  val cfi_is_ret = Bool()
  val ghist = UInt(globalHistoryLength.W)
  val lhist = UInt(localHistoryLength.W)
  val target = UInt(vaddrBitsExtended.W)
  val meta = UInt(bpdMaxMetaLength.W)
}

class BranchPredictorResponse(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val f1 = Vec(predWidth, new Prediction)
  val f2 = Vec(predWidth, new Prediction)
  val f3 = Vec(predWidth, new Prediction)
}


class RasUpdateInfo(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val attribute = new BranchAttribute
  val startPc = UInt(vaddrBitsExtended.W)
  val cfiPosition = UInt(log2Ceil(fetchBytes).W)
  val rasState = new RasState

}