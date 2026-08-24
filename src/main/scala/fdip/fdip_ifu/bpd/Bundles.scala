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

class BtbTrain(implicit p: Parameters) extends BpdBundle()(p)
  with HasBoomFrontendParameters
{
  val startPc = UInt(vaddrBitsExtended.W)
  val cfi     = new ResolvedCfi
  val meta    = Vec(l1btbParameters.nWays,
    Valid(new boom.fdip.ifu.bpd.l1btb.BTBPredictMeta()))
}

class TageCommitTrain(implicit p: Parameters) extends BpdBundle()(p)
  with HasBoomFrontendParameters
{
  val startPc = UInt(vaddrBitsExtended.W)
  val ghist   = new GlobalHistory
  val brMask  = UInt(predWidth.W)
  val cfi     = new ResolvedCfi
  val btbMeta = Vec(l1btbParameters.nWays,
    Valid(new boom.fdip.ifu.bpd.l1btb.BTBPredictMeta()))
  val meta    = new boom.fdip.ifu.bpd.tage.TagePredictMeta(tageParameters.tableInfo.size)
}

class FastTrain(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val startPc = UInt(vaddrBitsExtended.W)
  val branch  = new BranchInfo
}

class Train(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val startPc  = UInt(vaddrBitsExtended.W)
  val branch   = new BranchInfo
  val btbMeta  = Vec(BoomL1BTBParams().nWays,
    new boom.fdip.ifu.bpd.l1btb.BTBPredictMeta())
  val tageMeta = new boom.fdip.ifu.bpd.tage.TagePredictMeta(BoomTageParams().tableInfo.size)
  val ghist    = new GlobalHistory
}

class BranchPredictionMeta(
  microBtbParams: BoomFAMicroBTBParams = BoomFAMicroBTBParams(),
  btbParams: BoomL1BTBParams = BoomL1BTBParams(),
  tageParams: BoomTageParams = BoomTageParams()
)(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val tage = new boom.fdip.ifu.bpd.tage.TagePredictMeta(tageParams.tableInfo.size)
  val btb  = Vec(btbParams.nWays,
    Valid(new boom.fdip.ifu.bpd.l1btb.BTBPredictMeta()))
}

class BranchPredictionLiteBundle(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val pc          = UInt(vaddrBitsExtended.W)
  val valid       = Bool()
  val hasRedirect = Bool()
  val ftq_idx     = UInt((log2Ceil(ftqSz) + 1).W)
  val ghist       = new GlobalHistory
  val preds       = Vec(predWidth, new Prediction)
  val meta        = Output(new BranchPredictionMeta)
  val lhist       = Output(UInt(localHistoryLength.W))

  def predTakenVec = WireInit(VecInit((0 until predWidth) map { i =>
    valid && fetchMask(pc)(i) && preds(i).predicted_pc.valid &&
    (preds(i).is_jal || preds(i).is_jalr || preds(i).is_call || preds(i).is_ret ||
      (preds(i).is_br && preds(i).taken))
  }))

  def predTarget = {
    val takenIdx = PriorityEncoder(predTakenVec)
    val targets = VecInit(preds.map(_.predicted_pc.bits))
    Mux(predTakenVec(takenIdx), targets(takenIdx), nextFetch(pc))
  }

  def predGhist = {
    val takenIdx = PriorityEncoder(predTakenVec)
    val isBr = WireInit(VecInit(preds.map(p => p.is_br && p.predicted_pc.valid)))
    ghist.update(
      isBr.asUInt & fetchMask(pc),
      preds(takenIdx).taken && predTakenVec.reduce(_ || _),
      preds(takenIdx).is_br,
      takenIdx,
      predTakenVec.reduce(_ || _),
      pc,
      false.B,
      false.B)
  }

  def cfi_offset = {
    val offset = Wire(Valid(UInt(log2Ceil(predWidth).W)))
    offset.valid := predTakenVec.reduce(_ || _)
    offset.bits := PriorityEncoder(predTakenVec)
    offset
  }
}

class BranchPredictionRespBundle(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val f1 = new BranchPredictionLiteBundle
  val f2 = new BranchPredictionLiteBundle
  val f3 = new BranchPredictionLiteBundle
  val f3_pred_target = Output(UInt(vaddrBitsExtended.W))
  val f3_ras_top = Output(UInt(vaddrBitsExtended.W))

  def selectedResp = PriorityMux(Seq(
    (f3.valid && f3.hasRedirect) -> f3,
    (f2.valid && f2.hasRedirect) -> f2,
    f1.valid -> f1))

  def selectedRespIdxForFtq = PriorityMux(Seq(
    (f3.valid && f3.hasRedirect) -> BP_S3,
    (f2.valid && f2.hasRedirect) -> BP_S2,
    f1.valid -> BP_S1))

  def selectedFtqentry = {
    val res = Wire(new FTQPCBundle)
    val redirects = (0 until predWidth) map { i =>
      selectedResp.valid && fetchMask(selectedResp.pc)(i) && selectedResp.preds(i).predicted_pc.valid &&
      (selectedResp.preds(i).is_jal || selectedResp.preds(i).is_jalr ||
        selectedResp.preds(i).is_call || selectedResp.preds(i).is_ret ||
        (selectedResp.preds(i).is_br && selectedResp.preds(i).taken))
    }
    val redirectIdx = PriorityEncoder(redirects)
    val doRedirect = redirects.reduce(_ || _)
    val targets = VecInit(selectedResp.preds.map(_.predicted_pc.bits))
    val predictedTarget = Mux(doRedirect, targets(redirectIdx), nextFetch(selectedResp.pc))
    val brMask = WireInit(VecInit(selectedResp.preds.map(
      p => p.is_br || p.is_jal || p.is_jalr || p.is_call || p.is_ret)))
    val brTaken = WireInit(VecInit(selectedResp.preds.map(_.taken)))
    res.is_exit_branch := brMask.reduce(_ || _)
    res.is_exit_taken := selectedResp.predTakenVec.reduce(_ || _)
    res.exit_pc_offset := PriorityEncoder(selectedResp.predTakenVec)
    res.target := Mux(f3.valid && f3.hasRedirect, f3_pred_target, predictedTarget)
    res.start_pc := selectedResp.pc
    res.mask := fetchMask(res.start_pc) & MaskLower(UIntToOH(res.exit_pc_offset))
    res
  }

  def lastStage = f3
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
