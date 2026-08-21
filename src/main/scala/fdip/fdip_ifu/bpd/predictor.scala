package boom.fdip.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.fdip.common._
import boom.fdip.util._
import boom.perf._
import boom.fdip.ifu._
import java.lang



// A branch prediction for a single instruction
class BranchPrediction(implicit p: Parameters) extends BoomBundle()(p)
{
  // If this is a branch, do we take it?
  val taken           = Bool()

  // Is this a branch?
  val is_br           = Bool()
  // Is this a JAL?
  // val is_jal          = Bool()
  val is_call         = Bool()
  val is_ret          = Bool()
  val is_jalr         = Bool()//去除ret
  val is_jal          = Bool()//去除call

  val is_rvi_call     = Bool() // is the last instruction in a compressed call sequence
  // What is the target of his branch/jump? Do we know the target?
  val predicted_pc    = Valid(UInt(vaddrBitsExtended.W))


}

// The fixed FDIP BPU topology owns one training record per predictor.  Keeping
// these records separate makes predictor-local training state explicit at the
// FTQ boundary instead of relying on a shared bit-range convention.
class FAMicroBTBPredictMeta(val numWays: Int)(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val hits      = Vec(predWidth, Bool())
  val write_way = UInt(log2Ceil(numWays).W)
}

class BTBPredictMeta(val numWays: Int)(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val write_way = UInt(log2Ceil(numWays).W)
}

class TagePredictMeta(val nTables: Int)(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val provider     = Vec(predWidth, Valid(UInt(log2Ceil(nTables).W)))
  val alt_differs  = Vec(predWidth, Bool())
  val provider_u   = Vec(predWidth, UInt(2.W))
  val provider_ctr = Vec(predWidth, UInt(3.W))
  val allocate     = Vec(predWidth, Valid(UInt(log2Ceil(nTables).W)))
}

class BranchPredictionMeta(
  microBtbParams: BoomFAMicroBTBParams = BoomFAMicroBTBParams(),
  btbParams: BoomBTBParams = BoomBTBParams(),
  tageParams: BoomTageParams = BoomTageParams()
)(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  // Keep the fields in the same order as the fixed predictor pipeline.
  val tage     = new TagePredictMeta(tageParams.tableInfo.size)
  val btb      = new BTBPredictMeta(btbParams.nWays)
  val microBtb = new FAMicroBTBPredictMeta(microBtbParams.nWays)
  val bim      = new BIMMeta
}


class BranchPredictionLiteBundle(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val pc      = UInt(vaddrBitsExtended.W)
  val valid   = Bool()
  val hasRedirect = Bool()
  val ftq_idx = UInt((log2Ceil(ftqSz)+1).W)
  val ghist   = new GlobalHistory
  val preds   = Vec(predWidth, new BranchPrediction)
  val meta    = Output(new BranchPredictionMeta)
  val lhist   = Output(UInt(localHistoryLength.W))
  
  def predTakenVec = {
    val taken_vec = WireInit(VecInit((0 until predWidth) map { i =>
      valid && fetchMask(pc)(i) && preds(i).predicted_pc.valid &&
      (preds(i).is_jal || preds(i).is_jalr || preds(i).is_call || preds(i).is_ret ||
      (preds(i).is_br && preds(i).taken))
    }))
    taken_vec
  }
  def predTarget = {
    val taken_idx          = PriorityEncoder(predTakenVec)
    //没有taken，但地址不是nextfetch为错误行为
    val targs              = VecInit(preds.map(_.predicted_pc.bits))
    val predicted_target   = Mux(predTakenVec(taken_idx),
                                  targs(taken_idx),
                                  nextFetch(pc))
    predicted_target
  }
  // only used for s1 s2 stage
  def predGhist =  {
    val taken_idx          = PriorityEncoder(predTakenVec)
    val is_br_vec          = WireInit(VecInit(preds.map(p => p.is_br && p.predicted_pc.valid)))
    val predicted_ghist = ghist.update(
      is_br_vec.asUInt & fetchMask(pc),
      preds(taken_idx).taken && predTakenVec.reduce(_||_),
      preds(taken_idx).is_br,
      taken_idx,
      predTakenVec.reduce(_||_),
      pc,
      false.B,//!preds(taken_idx).is_jal,
      false.B // no need to consider jalr
    )
    predicted_ghist
  }
  def cfi_offset = {
    val offset  = Wire(Valid(UInt(log2Ceil(predWidth).W)))
    offset.valid := predTakenVec.reduce(_||_)
    offset.bits  := PriorityEncoder(predTakenVec)
    offset
  }
} 
class BranchPredictionRespBundle(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val f1 = (new BranchPredictionLiteBundle)
  val f2 = (new BranchPredictionLiteBundle)
  val f3 = (new BranchPredictionLiteBundle)
  // val f3_predicted_ghist = Output(new GlobalHistory)
  val f3_pred_target     = Output(UInt(vaddrBitsExtended.W))
  val f3_ras_top         = Output(UInt(vaddrBitsExtended.W))
  def selectedResp = {
    val res =
      PriorityMux(Seq(
        (f3.valid && f3.hasRedirect) -> f3,
        (f2.valid && f2.hasRedirect) -> f2,
        f1.valid                     -> f1
      ))
    res
  }
  def selectedRespIdxForFtq =
    PriorityMux(Seq(
      (f3.valid && f3.hasRedirect) -> BP_S3,
      (f2.valid && f2.hasRedirect) -> BP_S2,
      f1.valid                     -> BP_S1
    ))
  def selectedFtqentry = {
    val res = Wire(new FTQPCBundle)
    val redirects = (0 until predWidth) map { i =>
      selectedResp.valid && fetchMask(selectedResp.pc)(i) && selectedResp.preds(i).predicted_pc.valid &&
      (selectedResp.preds(i).is_jal || selectedResp.preds(i).is_jalr || selectedResp.preds(i).is_call ||selectedResp.preds(i).is_ret|| 
        (selectedResp.preds(i).is_br && selectedResp.preds(i).taken))
    }
    val redirect_idx = PriorityEncoder(redirects)
    val do_redirect = redirects.reduce(_||_)
    val targs = VecInit(selectedResp.preds.map(_.predicted_pc.bits))
    val predicted_target = Mux(do_redirect,
                                  targs(redirect_idx),
                                  nextFetch(selectedResp.pc))
    val br_mask         = WireInit(VecInit(selectedResp.preds.map(i=>i.is_br||i.is_jal||i.is_jalr||i.is_call||i.is_ret)))
    val br_taken        = WireInit(VecInit(selectedResp.preds.map(i=>i.taken)))
    res.is_exit_branch := br_mask.reduce(_||_)
    res.is_exit_taken  := selectedResp.predTakenVec.reduce(_||_)
    res.exit_pc_offset := PriorityEncoder(selectedResp.predTakenVec)
    res.target         := Mux(f3.valid && f3.hasRedirect,f3_pred_target,predicted_target)
    res.start_pc       := selectedResp.pc
    res.mask           := fetchMask(res.start_pc)&MaskLower(UIntToOH(res.exit_pc_offset))
    res
  }
  def lastStage = f3
}

// A branch update for a fetch-width worth of instructions
class BranchPredictionUpdate(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  // Indicates that this update is due to a speculated misprediction
  // Local predictors typically update themselves with speculative info
  // Global predictors only care about non-speculative updates
  val is_mispredict_update = Bool()
  val is_repair_update = Bool()
  val btb_mispredicts = UInt(predWidth.W)
  def is_btb_mispredict_update = btb_mispredicts =/= 0.U
  def is_commit_update = !(is_mispredict_update || is_repair_update || is_btb_mispredict_update)

  val pc            = UInt(vaddrBitsExtended.W)
  // Mask of instructions which are branches.
  // If these are not cfi_idx, then they were predicted not taken
  val br_mask       = UInt(predWidth.W)
  // Which CFI was taken/mispredicted (if any)
  val cfi_idx       = Valid(UInt(log2Ceil(predWidth).W))
  // Was the cfi taken?
  val cfi_taken     = Bool()
  // Was the cfi mispredicted from the original prediction?
  val cfi_mispredicted = Bool()
  // Was the cfi a br?
  val cfi_is_br     = Bool()
  // Was the cfi a jal/jalr?
  val cfi_is_jal  = Bool()
  // Was the cfi a jalr
  val cfi_is_jalr = Bool()
  val cfi_is_ret  = Bool()
  val cfi_is_call = Bool()

  val ghist = new GlobalHistory
  val lhist = UInt(localHistoryLength.W)


  // What did this CFI jump to?
  val target        = UInt(vaddrBitsExtended.W)

  val meta          = new BranchPredictionMeta
}

// A branch update to a single predictor.
class BranchPredictorUpdate(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val is_mispredict_update     = Bool()
  val is_repair_update         = Bool()

  val btb_mispredicts  = UInt(predWidth.W)
  def is_btb_mispredict_update = btb_mispredicts =/= 0.U

  def is_commit_update = !(is_mispredict_update || is_repair_update || is_btb_mispredict_update)

  val pc               = UInt(vaddrBitsExtended.W)

  val br_mask          = UInt(predWidth.W)
  val cfi_idx          = Valid(UInt(log2Ceil(predWidth).W))
  val cfi_taken        = Bool()
  val cfi_mispredicted = Bool()

  val cfi_is_br        = Bool()
  val cfi_is_jal       = Bool()
  val cfi_is_jalr      = Bool()
  val cfi_is_call      = Bool()
  val cfi_is_ret       = Bool()

  val ghist            = UInt(globalHistoryLength.W)
  val lhist            = UInt(localHistoryLength.W)

  val target           = UInt(vaddrBitsExtended.W)

  val meta             = UInt(bpdMaxMetaLength.W)
}

class BranchPredictionRequest(implicit p: Parameters) extends BoomBundle()(p)
{
  val pc    = UInt(vaddrBitsExtended.W)
  // val ghist = new GlobalHistory
}


class BranchPredictorResponse(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val f1 = Vec(predWidth, new BranchPrediction)
  val f2 = Vec(predWidth, new BranchPrediction)
  val f3 = Vec(predWidth, new BranchPrediction)
}

abstract class AbstractBranchPredictor(implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFrontendParameters
{
  val metaSz   = 0
  def nInputs  = 1
  val mems: Seq[Tuple3[String, Int, Int]]

  val io = IO(new Bundle {
    val f0_valid = Input(Bool())
    val f0_pc    = Input(UInt(vaddrBitsExtended.W))
    val f0_mask  = Input(UInt(predWidth.W))
    // Local history not available until end of f1
    val f1_ghist = Input(UInt(globalHistoryLength.W))
    val f1_lhist = Input(UInt(localHistoryLength.W))

    val resp_in = Input(Vec(nInputs, new BranchPredictorResponse))
    val resp = Output(new BranchPredictorResponse)

    // Store the meta as a UInt, use width inference to figure out the shape
    val f3_meta = Output(UInt(bpdMaxMetaLength.W))

    val f3_fire = Input(Bool())

    val update = Input(Valid(new BranchPredictorUpdate))
  })
  io.resp := io.resp_in(0)

  io.f3_meta := 0.U

  val s0_idx       = fetchIdx(io.f0_pc)
  val s1_idx       = RegNext(s0_idx)
  val s2_idx       = RegNext(s1_idx)
  val s3_idx       = RegNext(s2_idx)

  val s0_valid      = io.f0_valid
  val s1_valid      = RegNext(s0_valid)
  val s2_valid      = RegNext(s1_valid)
  val s3_valid      = RegNext(s2_valid)

  val s0_mask       = io.f0_mask
  val s1_mask       = RegNext(s0_mask)
  val s2_mask       = RegNext(s1_mask)
  val s3_mask       = RegNext(s2_mask)

  val s0_pc         = io.f0_pc
  val s1_pc         = RegNext(s0_pc)

  val s0_update     = io.update
  val s0_update_idx = fetchIdx(io.update.bits.pc)
  val s0_update_valid = io.update.valid

  val s1_update     = RegNext(s0_update)
  val s1_update_idx = RegNext(s0_update_idx)
  val s1_update_valid = RegNext(s0_update_valid)
}

/* 
负责next pc和ghist维护

input :predWidth
process:predWidth
output:predWidth
predWidth <= predWidth
 */
class BranchPredictor(implicit p: Parameters) extends BoomModule()(p)
 with HasBoomFrontendParameters
{
  val io = IO(new Bundle {

    // Requests and responses
    // val f0_req = Input(Valid(new BranchPredictionRequest))
    val fromFtq       = Flipped(new FTQToBPUIO)
    val toFtq         = new BPUToFTQIO
    val reset_vector  = Input(UInt(vaddrBitsExtended.W))
    // val resp = Output(new Bundle {
    //   val f1 = new BranchPredictionBundle
    //   val f2 = new BranchPredictionBundle
    //   val f3 = new BranchPredictionBundle
    // })

    val f3_fire = Input(Bool())

    // Update
    // val update = Input(Valid(new BranchPredictionUpdate))
  })

  //TODO: BTB need to record call or ret so that we can use RAS and predict update
  val ras               = Module(new BoomRAS)
//如果ftq没有空间，那么s1阶段会一直保持这个pc，并且持续预测（或者使用fire发送预测请求？）
/* 
由于送入bpu的是s0阶段的请求，如果s1阶段没有fire，
 */
  val s0_pc             = WireInit(io.reset_vector)
  val s0_valid          = WireInit(false.B)
  val s0_ghist          = WireInit((0.U).asTypeOf(new GlobalHistory))
  val s0_fire, s1_fire, s2_fire, s3_fire = WireInit(false.B)
  //由于s1阶段可能由于ftq full导致预测信息无法写入，此时需要去replay pc和ghist
  val s1_replay          = WireInit(false.B)
  
  val s1_ready, s2_ready, s3_ready = WireInit(false.B)
  dontTouch(s0_valid)
  dontTouch(s0_ghist)
  val s1_pc             = RegEnable(s0_pc,s0_fire)
  val s1_clear          = WireInit(false.B)
  val s1_valid          = RegInit(false.B)
  val s1_ghist          = RegEnable(s0_ghist,s0_fire)
  // val s1_target         = Wire(UInt(vaddrBitsExtended.W))

  val s2_pc             = RegEnable(s1_pc,s1_fire)
  val s2_valid          = RegInit(false.B)//RegEnable(s1_valid,false.B,io.toFtq.resp.ready&&(!s1_clear))
  val s2_redirect       = WireInit(false.B)
  val s2_clear          = WireInit(false.B)
  val s2_ghist          = RegEnable(s1_ghist,s1_fire)
  // val s2_target         = Wire(UInt(vaddrBitsExtended.W))
  val s2_resp           = WireInit(io.toFtq.resp.bits.f2)

  val s3_pc             = RegEnable(s2_pc,s2_fire)
  val s3_valid          = RegInit(false.B)//RegEnable(s2_valid,false.B,s2_fire&&(!s2_clear))
  val s3_clear          = WireInit(false.B)
  val s3_redirect       = WireInit(false.B)
  val s3_ghist          = RegEnable(s2_ghist,s2_fire)
  val s3_resp           = WireInit(io.toFtq.resp.bits.f3)

  // Fixed predictor topology: FAMicroBTB -> BIM -> BTB -> TAGE.
  val microBtb = Module(new FAMicroBTBBranchPredictor())
  val bim      = Module(new BIMBranchPredictor())
  val btb      = Module(new BTBBranchPredictor())
  val tage     = Module(new TageBranchPredictor())

  val bpdF0Valid = s0_valid
  val bpdF0Pc    = fetchAlign(s0_pc)
  val bpdF0Mask  = fetchMask(s0_pc)
  val bpdF1Ghist = RegNext(s0_ghist.histories)
  val bpdF3Fire  = false.B

  microBtb.io.f0_valid := bpdF0Valid
  microBtb.io.f0_pc    := bpdF0Pc
  microBtb.io.f0_mask  := bpdF0Mask
  microBtb.io.f1_ghist := bpdF1Ghist
  microBtb.io.f1_lhist := DontCare
  microBtb.io.f3_fire  := bpdF3Fire

  bim.io.f0_valid := bpdF0Valid
  bim.io.f0_pc    := bpdF0Pc
  bim.io.f0_mask  := bpdF0Mask
  bim.io.f1_ghist := bpdF1Ghist
  bim.io.f1_lhist := DontCare
  bim.io.f3_fire  := bpdF3Fire

  btb.io.f0_valid := bpdF0Valid
  btb.io.f0_pc    := bpdF0Pc
  btb.io.f0_mask  := bpdF0Mask
  btb.io.f1_ghist := bpdF1Ghist
  btb.io.f1_lhist := DontCare
  btb.io.f3_fire  := bpdF3Fire

  tage.io.f0_valid := bpdF0Valid
  tage.io.f0_pc    := bpdF0Pc
  tage.io.f0_mask  := bpdF0Mask
  tage.io.f1_ghist := bpdF1Ghist
  tage.io.f1_lhist := DontCare
  tage.io.f3_fire  := bpdF3Fire

  val emptyBpdResp = WireInit(0.U.asTypeOf(new BranchPredictorResponse))
  microBtb.io.resp_in(0) := emptyBpdResp
  bim.io.resp_in(0)      := microBtb.io.resp
  btb.io.resp_in(0)      := bim.io.resp
  tage.io.resp_in(0)     := btb.io.resp
  val bpdResp = tage.io.resp

  val bpdMeta = Wire(new BranchPredictionMeta)
  bpdMeta.tage     := tage.io.f3_meta.asTypeOf(new TagePredictMeta(tage.tageNTables))
  bpdMeta.btb      := btb.io.f3_meta.asTypeOf(new BTBPredictMeta(btb.nWays))
  bpdMeta.microBtb := microBtb.io.f3_meta.asTypeOf(new FAMicroBTBPredictMeta(microBtb.nWays))
  bpdMeta.bim      := bim.io.f3_meta.asTypeOf(new BIMMeta)
  //实质上，只要s1阶段fire，那么s2，s3必然valid,

  //TODO:delete handshake signal
  s0_fire               := s0_valid && s1_ready
  s1_fire               := s1_valid && s2_ready && io.toFtq.resp.ready
  s2_fire               := s2_valid && s3_ready
  s3_fire               := s3_valid  
// s1阶段ready的条件：s1阶段fire，或者s1阶段无效，或者s1阶段clear（此时s1阶段数据无效）
  s1_ready              := s1_fire || !s1_valid || s1_clear
  s2_ready              := s2_fire || !s2_valid || s2_clear
  s3_ready              := s3_fire || !s3_valid || s3_clear
  
  //当clear信号来临，s1_valid 不需要被清除，因为此时s0_valid就是redirect信号，其他阶段无所谓
  //s2，s3阶段发生clear，需要清除自身的valid，假如s2发生重定向，实际上就是下一个到s2阶段的就是无效信号
  when(s0_fire){
    s1_valid := true.B
  }.elsewhen(s1_fire){
    s1_valid := false.B
  }

  when(s1_clear){
    s2_valid := false.B
  }.elsewhen(s1_fire ){
    s2_valid := true.B
  }.elsewhen(s2_fire){
    s2_valid := false.B
  }

//如果s2阶段发生clear，
  when(s2_clear){
    s3_valid := false.B
  }.elsewhen(s2_fire ){
    s3_valid := true.B
  }.elsewhen(s3_fire){
    s3_valid := false.B
  }
  s1_replay := s1_valid && !(s2_ready && io.toFtq.resp.ready)
  // val s3_target         = Wire(UInt(vaddrBitsExtended.W))
  val s3_align_pc       = fetchAlign(s3_pc)
  val s3_taken_vec = WireInit(VecInit((0 until predWidth) map { i =>
    s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
    (s3_resp.preds(i).is_jal||s3_resp.preds(i).is_jalr ||s3_resp.preds(i).is_call||s3_resp.preds(i).is_ret ||
    (s3_resp.preds(i).is_br && s3_resp.preds(i).taken))
  }))
  val s3_taken_is_call = WireInit(VecInit((0 until predWidth) map { i =>
    s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
    (s3_resp.preds(i).is_call)
  }))
  val s3_taken_is_rvi_call = WireInit(VecInit((0 until predWidth) map { i =>
    s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
    (s3_resp.preds(i).is_call)&&s3_resp.preds(i).is_rvi_call
  }))
  val s3_taken_is_ret = WireInit(VecInit((0 until predWidth) map { i =>
    s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
    (s3_resp.preds(i).is_ret)
  }))
  
  val s3_is_br     = WireInit(VecInit((0 until predWidth) map { i =>
    s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
    (s3_resp.preds(i).is_br)
  }))
  // val s3_taken_idx = 
  val s3_predTarget = Mux(s3_taken_vec.reduce(_||_),
    Mux(s3_taken_is_ret(PriorityEncoder(s3_taken_vec)) && useRAS.B,
      ras.io.read_addr,
      s3_resp.preds(PriorityEncoder(s3_taken_vec)).predicted_pc.bits
    ),
    nextFetch(s3_pc)
  )

  val s3_predGhist = s3_resp.ghist.update(
    s3_is_br.asUInt,
    s3_taken_vec.reduce(_||_),
    s3_resp.preds(PriorityEncoder(s3_taken_vec)).is_br,
    PriorityEncoder(s3_taken_vec),
    s3_taken_vec.reduce(_||_),
    s3_pc,
    s3_taken_is_call(PriorityEncoder(s3_taken_vec)),
    s3_taken_is_ret(PriorityEncoder(s3_taken_vec))
  )

  

  val s1_previous_pred_info    = RegNext(io.toFtq.resp.bits.f1)
  val s2_previous_pred_info    = RegNext(io.toFtq.resp.bits.f2)
  val s3_saw_s1_pred_info      = RegNext(s1_previous_pred_info)
  val f2_correct_f1_ghist     = WireInit(s1_previous_pred_info.predGhist =/=  io.toFtq.resp.bits.f2.predGhist&& enableGHistStallRepair.B && s2_valid)
  val f3_correct_f2_ghist     = WireInit(s2_previous_pred_info.predGhist =/=  io.toFtq.resp.bits.f3.predGhist&& enableGHistStallRepair.B && s3_valid)
  val f3_correct_f1_ghist     = WireInit(s3_saw_s1_pred_info.predGhist   =/=  io.toFtq.resp.bits.f3.predGhist&& enableGHistStallRepair.B && s3_valid)
  dontTouch(s1_replay)
  dontTouch(f2_correct_f1_ghist)
  dontTouch(f3_correct_f2_ghist)
  dontTouch(f3_correct_f1_ghist)
  s0_pc                 := MuxCase(io.reset_vector,
  Seq(
    (RegNext(reset.asBool) && !reset.asBool) -> io.reset_vector,
    io.fromFtq.redirect_val                  -> io.fromFtq.redirect_pc,
    (s3_redirect || f3_correct_f2_ghist)     -> s3_predTarget,
    (s2_redirect || f2_correct_f1_ghist)     -> io.toFtq.resp.bits.f2.predTarget,
    s1_replay                                -> s1_pc,
    s1_valid                                 -> io.toFtq.resp.bits.f1.predTarget
  )
  )
  s0_valid              := (RegNext(reset.asBool) && !reset.asBool) || io.fromFtq.redirect_val ||
                          s3_redirect || s2_redirect || s1_valid ||s1_replay ||f3_correct_f2_ghist || f2_correct_f1_ghist
  s0_ghist              := MuxCase((0.U).asTypeOf(new GlobalHistory),
  Seq(
    (RegNext(reset.asBool) && !reset.asBool) -> (0.U).asTypeOf(new GlobalHistory),
    io.fromFtq.redirect_val                  -> io.fromFtq.redirect_ghist,
    (s3_redirect||f3_correct_f2_ghist)       -> s3_predGhist,
    (s2_redirect||f2_correct_f1_ghist)       -> io.toFtq.resp.bits.f2.predGhist,
    s1_replay                                -> s1_ghist,                     
    s1_valid                                 -> io.toFtq.resp.bits.f1.predGhist
  )
  )
  //redirect and clear
  //其实这里还需要去做位置判断
  s2_redirect                 := io.toFtq.resp.bits.f2.valid&&(s1_previous_pred_info.valid)&&(s1_previous_pred_info.predTarget=/=io.toFtq.resp.bits.f2.predTarget)&&s2_valid
  s3_redirect                 := io.toFtq.resp.bits.f3.valid&&(s2_previous_pred_info.valid)&&(s2_previous_pred_info.predTarget=/=io.toFtq.resp.bits.f3.predTarget)&&s3_valid
  

 
  s1_clear                    := io.fromFtq.redirect_val || (s3_redirect||f3_correct_f2_ghist) || (s2_redirect||f2_correct_f1_ghist) 
  s2_clear                    := io.fromFtq.redirect_val || (s3_redirect||f3_correct_f2_ghist)
  s3_clear                    := io.fromFtq.redirect_val

  io.toFtq.resp.bits.f3_ras_top     := ras.io.read_addr
  io.toFtq.resp.bits.f1.hasRedirect := false.B
  io.toFtq.resp.bits.f2.hasRedirect := (s2_redirect||f2_correct_f1_ghist) && (! s2_clear)
  io.toFtq.resp.bits.f3.hasRedirect := (s3_redirect||f3_correct_f2_ghist) && (! s3_clear)
  io.toFtq.resp.bits.f3_pred_target := s3_predTarget

  //TODO:add update for RAS
  ras.io.read_idx                   := s3_resp.ghist.ras_idx

//predict update
  ras.io.write_valid := s3_valid && io.f3_fire && useRAS.B && s3_taken_is_call(PriorityEncoder(s3_taken_vec))
  ras.io.write_addr  := s3_align_pc + (PriorityEncoder(s3_taken_vec) << 1) + Mux(
    s3_taken_is_rvi_call(PriorityEncoder(s3_taken_vec)), 4.U, 2.U)
  ras.io.write_idx   := WrapInc(s3_resp.ghist.ras_idx, nRasEntries)
  // //这里最主要的意思就是去一个周期2 branch
/* ----------------------------------- Req ---------------------------------- */
  // def predMask(pc: UInt) = {
  //   val base = fetchAlign(pc)
  //   val idx = base.extract(log2Ceil(predWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))//这里要求predWidth小于等于8
  //   val mask = WireInit(0.U(predWidth.W))
  //   mask := ((1 << predWidth)-1).U << idx
  //   mask
  // }
/* ---------------------------------- Resp ---------------------------------- */

  dontTouch(s1_previous_pred_info)
  io.toFtq.resp.bits.f1.preds    := bpdResp.f1
  io.toFtq.resp.bits.f2.preds    := bpdResp.f2
  io.toFtq.resp.bits.f3.preds    := bpdResp.f3
  io.toFtq.resp.bits.f3.meta     := bpdMeta


  io.toFtq.resp.bits.f1.pc := RegNext(s0_pc)
  io.toFtq.resp.bits.f2.pc := RegNext(s1_pc)
  io.toFtq.resp.bits.f3.pc := RegNext(s2_pc)

  io.toFtq.resp.valid         := s1_fire && (!s1_clear) 
  io.toFtq.resp.bits.f1.valid := s1_fire && (!s1_clear) 
  io.toFtq.resp.bits.f1.ghist := s1_ghist

  io.toFtq.resp.bits.f1.ftq_idx:= DontCare
  

  io.toFtq.resp.bits.f2.valid   := s2_fire && (!s2_clear)
  io.toFtq.resp.bits.f2.ftq_idx := RegNext(io.fromFtq.ftq_idx)
  io.toFtq.resp.bits.f2.ghist   := s2_ghist

  io.toFtq.resp.bits.f3.valid := s3_fire && (!s3_clear)
  io.toFtq.resp.bits.f3.ftq_idx:= RegNext(RegNext(io.fromFtq.ftq_idx))
  io.toFtq.resp.bits.f3.ghist := s3_ghist


  // We don't care about meta from the f1 and f2 resps
  // Use the meta from the latest resp
  io.toFtq.resp.bits.f1.meta  := DontCare
  io.toFtq.resp.bits.f2.meta  := DontCare
  io.toFtq.resp.bits.f1.lhist := DontCare
  io.toFtq.resp.bits.f2.lhist := DontCare
  io.toFtq.resp.bits.f3.lhist := DontCare
/* --------------------------------- update --------------------------------- */
  val bpdUpdate = Wire(Valid(new BranchPredictorUpdate))
  bpdUpdate.bits.is_mispredict_update := io.fromFtq.bpdupdate.bits.is_mispredict_update
  bpdUpdate.bits.is_repair_update     := io.fromFtq.bpdupdate.bits.is_repair_update
  bpdUpdate.bits.meta                  := 0.U
  bpdUpdate.bits.lhist                 := io.fromFtq.bpdupdate.bits.lhist
  bpdUpdate.bits.cfi_idx.bits          := io.fromFtq.bpdupdate.bits.cfi_idx.bits
  bpdUpdate.bits.cfi_taken             := io.fromFtq.bpdupdate.bits.cfi_taken
  bpdUpdate.bits.cfi_mispredicted      := io.fromFtq.bpdupdate.bits.cfi_mispredicted
  bpdUpdate.bits.cfi_is_br             := io.fromFtq.bpdupdate.bits.cfi_is_br
  bpdUpdate.bits.cfi_is_jal            := io.fromFtq.bpdupdate.bits.cfi_is_jal
  bpdUpdate.bits.cfi_is_jalr           := io.fromFtq.bpdupdate.bits.cfi_is_jalr
  bpdUpdate.bits.cfi_is_ret            := io.fromFtq.bpdupdate.bits.cfi_is_ret
  bpdUpdate.bits.cfi_is_call           := io.fromFtq.bpdupdate.bits.cfi_is_call
  bpdUpdate.bits.target                := io.fromFtq.bpdupdate.bits.target
  bpdUpdate.valid                      := io.fromFtq.bpdupdate.valid
  bpdUpdate.bits.pc                    := fetchAlign(io.fromFtq.bpdupdate.bits.pc)
  bpdUpdate.bits.br_mask               := io.fromFtq.bpdupdate.bits.br_mask
  bpdUpdate.bits.btb_mispredicts       := io.fromFtq.bpdupdate.bits.btb_mispredicts
  bpdUpdate.bits.cfi_idx.valid         := io.fromFtq.bpdupdate.bits.cfi_idx.valid
  bpdUpdate.bits.ghist                 := io.fromFtq.bpdupdate.bits.ghist.histories

  microBtb.io.update := bpdUpdate
  bim.io.update      := bpdUpdate
  btb.io.update      := bpdUpdate
  tage.io.update     := bpdUpdate

  bim.io.update.bits.meta      := io.fromFtq.bpdupdate.bits.meta.bim.asUInt
  microBtb.io.update.bits.meta := io.fromFtq.bpdupdate.bits.meta.microBtb.asUInt
  btb.io.update.bits.meta      := io.fromFtq.bpdupdate.bits.meta.btb.asUInt
  tage.io.update.bits.meta     := io.fromFtq.bpdupdate.bits.meta.tage.asUInt

  XSPerfAccumulate("f2_correct_f1_ghist",   f2_correct_f1_ghist)
  XSPerfAccumulate("f3_correct_f2_ghist",   f3_correct_f2_ghist)
  XSPerfAccumulate("f3_correct_f1_ghist",   f3_correct_f1_ghist)

  val commit_ghist        = RegInit(0.U.asTypeOf(new GlobalHistory))
  dontTouch(commit_ghist)
  when (io.fromFtq.bpdupdate.valid && bpdUpdate.bits.is_commit_update) {
    commit_ghist := commit_ghist.update(
      bpdUpdate.bits.br_mask,
      bpdUpdate.bits.cfi_taken,
      bpdUpdate.bits.cfi_is_br,
      bpdUpdate.bits.cfi_idx.bits,
      bpdUpdate.bits.cfi_idx.valid,
      bpdUpdate.bits.pc,
      bpdUpdate.bits.cfi_is_call,
      bpdUpdate.bits.cfi_is_ret
    )
    // assert(commit_ghist.histories === bpdUpdate.bits.ghist,
    //   "Branch Predictor Commit Update GHist does not match the expected value!")
  }
}
