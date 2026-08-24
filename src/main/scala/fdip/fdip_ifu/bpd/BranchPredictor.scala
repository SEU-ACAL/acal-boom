package boom.fdip.ifu.bpd

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.fdip.common._
import boom.fdip.util._
import boom.perf._
import boom.fdip.ifu._
import boom.fdip.ifu.bpd.ubtb._
import boom.fdip.ifu.bpd.l1btb._
import boom.fdip.ifu.bpd.tage._
import boom.fdip.ifu.bpd.ras._
import java.lang


class BranchPredictor(implicit p: Parameters) extends BpdModule()(p)
{
  val io = IO(new Bundle {

    // Requests and responses
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

  // Fixed predictor topology: FAMicroBTB -> BTB -> TAGE.
  val microBtb = Module(new FAMicroBTBBranchPredictor())
  val btb      = Module(new L1BTB())
  val tage     = Module(new TageBranchPredictor())

  val bpdF0Valid = s0_valid
  val bpdF0Pc    = fetchAlign(s0_pc)
  val bpdF1Ghist = RegNext(s0_ghist.histories)

  microBtb.io.f0_valid := bpdF0Valid
  microBtb.io.f0_pc    := bpdF0Pc
  // microBtb.io.train.valid := s3_valid && io.f3_fire && s3_resp.valid
  // microBtb.io.train.bits := s3_resp.preds(PriorityEncoder(s3_resp.predTakenVec))
  // microBtb.io.train.bits.startPc := s3_pc
  // microBtb.io.train.bits.branch.target := s3_predTarget
  // microBtb.io.train.bits.branch.cfiPosition := s3_resp.cfi_offset.bits
  // microBtb.io.train.bits.branch.attribute := s3_resp.preds(PriorityEncoder(s3_resp.predTakenVec)).attribute
  // microBtb.io.train.bits.branch.taken := s3_resp.preds(PriorityEncoder(s3_resp.predTakenVec)).taken
  // microBtb.io.train.bits.branch.mispredict := s3_redirect


  btb.io.f0_valid := bpdF0Valid
  btb.io.f0_pc    := bpdF0Pc
  btb.io.train    := io.fromFtq.btbTrain

  tage.io.f0_valid := bpdF0Valid
  tage.io.f0_pc    := bpdF0Pc
  tage.io.f1_ghist := bpdF1Ghist
  tage.io.train    := io.fromFtq.tageCommitTrain

  btb.io.resp_in      := microBtb.io.resp
  tage.io.resp_in     := btb.io.resp
  val bpdResp = tage.io.resp

  val bpdMeta = Wire(new BranchPredictionMeta)
  bpdMeta.tage     := tage.io.f3_meta
  bpdMeta.btb      := btb.io.f3_meta
  // bpdMeta.microBtb := microBtb.io.f3_meta
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


  // val s3_taken_vec = WireInit(VecInit((0 until predWidth) map { i =>
  //   s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
  //   (s3_resp.preds(i).is_jal||s3_resp.preds(i).is_jalr ||s3_resp.preds(i).is_call||s3_resp.preds(i).is_ret ||
  //   (s3_resp.preds(i).is_br && s3_resp.preds(i).taken))
  // }))
  // val s3_taken_is_call = WireInit(VecInit((0 until predWidth) map { i =>
  //   s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
  //   (s3_resp.preds(i).is_call)
  // }))
  // val s3_taken_is_rvi_call = WireInit(VecInit((0 until predWidth) map { i =>
  //   s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
  //   (s3_resp.preds(i).is_call)&&s3_resp.preds(i).is_rvi_call
  // }))
  // val s3_taken_is_ret = WireInit(VecInit((0 until predWidth) map { i =>
  //   s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
  //   (s3_resp.preds(i).is_ret)
  // }))
  
  // val s3_is_br     = WireInit(VecInit((0 until predWidth) map { i =>
  //   s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
  //   (s3_resp.preds(i).is_br)
  // }))
  // // val s3_taken_idx = 
  // val s3_predTarget = Mux(s3_taken_vec.reduce(_||_),
  //   Mux(s3_taken_is_ret(PriorityEncoder(s3_taken_vec)) && useRAS.B,
  //     ras.io.read_addr,
  //     s3_resp.preds(PriorityEncoder(s3_taken_vec)).predicted_pc.bits
  //   ),
  //   nextFetch(s3_pc)
  // )

  // val s3_predGhist = s3_resp.ghist.update(
  //   s3_is_br.asUInt,
  //   s3_taken_vec.reduce(_||_),
  //   s3_resp.preds(PriorityEncoder(s3_taken_vec)).is_br,
  //   PriorityEncoder(s3_taken_vec),
  //   s3_taken_vec.reduce(_||_),
  //   s3_pc,
  //   s3_taken_is_call(PriorityEncoder(s3_taken_vec)),
  //   s3_taken_is_ret(PriorityEncoder(s3_taken_vec))
  // )

  

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
  XSPerfAccumulate("f2_correct_f1_ghist",   f2_correct_f1_ghist)
  XSPerfAccumulate("f3_correct_f2_ghist",   f3_correct_f2_ghist)
  XSPerfAccumulate("f3_correct_f1_ghist",   f3_correct_f1_ghist)

  val commit_ghist        = RegInit(0.U.asTypeOf(new GlobalHistory))
  dontTouch(commit_ghist)
  when (io.fromFtq.bpdupdate.valid && io.fromFtq.bpdupdate.bits.is_commit_update) {
    commit_ghist := commit_ghist.update(
      io.fromFtq.bpdupdate.bits.br_mask,
      io.fromFtq.bpdupdate.bits.cfi_taken,
      io.fromFtq.bpdupdate.bits.cfi_is_br,
      io.fromFtq.bpdupdate.bits.cfi_idx.bits,
      io.fromFtq.bpdupdate.bits.cfi_idx.valid,
      fetchAlign(io.fromFtq.bpdupdate.bits.pc),
      io.fromFtq.bpdupdate.bits.cfi_is_call,
      io.fromFtq.bpdupdate.bits.cfi_is_ret
    )
    // assert(commit_ghist.histories === io.fromFtq.bpdupdate.bits.ghist.histories,
    //   "Branch Predictor Commit Update GHist does not match the expected value!")
  }
}

