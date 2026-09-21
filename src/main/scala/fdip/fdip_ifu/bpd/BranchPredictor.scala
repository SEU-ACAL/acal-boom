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
import boom.fdip.ifu.bpd.common._
import java.lang
import boom.fdip.ifu.bpd.ubtb._
import boom.fdip.ifu.bpd.l1btb._
import boom.fdip.ifu.bpd.tage._
import boom.fdip.ifu.bpd.ras._
//TODO: add taken cmp
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

  class BpuPredictionTrace extends Bundle {
    val startPc = UInt(vaddrBitsExtended.W)
    val f1Taken = Bool()
    val f1Position = UInt(log2Ceil(fetchBytes).W)
    val f2Taken = Bool()
    val f2Position = UInt(log2Ceil(fetchBytes).W)
    val f2Target = UInt(vaddrBitsExtended.W)
    val s2Redirect = Bool()
    val ghistRepair = Bool()
  }

  class BpuTrainTrace extends Bundle {
    val startPc = UInt(vaddrBitsExtended.W)
    val cfiValid = Bool()
    val cfiPosition = UInt(log2Ceil(fetchBytes).W)
    val actualTaken = Bool()
    val mispredict = Bool()
    val attribute = UInt(io.fromFtq.train.bits.cfi.attribute.getWidth.W)
  }

  private val predictionTrace = ChiselDB.createTable("BpuPredictionTrace", new BpuPredictionTrace)
  private val trainTrace = ChiselDB.createTable("BpuTrainTrace", new BpuTrainTrace)

  //TODO: BTB need to record call or ret so that we can use RAS and predict update
  // Fixed predictor topology: FAMicroBTB -> BTB -> TAGE.
  val microBtb = Module(new FAMicroBTBBranchPredictor())
  val btb      = Module(new L1BTB())
  val tage     = Module(new TageBranchPredictor())
  val ras               = Module(new BoomRAS)
  val phr               = Module(new PHR)
//如果ftq没有空间，那么s1阶段会一直保持这个pc，并且持续预测（或者使用fire发送预测请求？）
/* 
由于送入bpu的是s0阶段的请求，如果s1阶段没有fire，
*/

  val s0_pc             = WireInit(io.reset_vector)
  val s0_valid          = WireInit(false.B)
  val s0_phr            = phr.io.s0_phr
  val s0_rasState       = WireInit((0.U).asTypeOf(new RasState))
  val bpdF0Valid = s0_valid
  val bpdF0Pc    = s0_pc

  val s0_fire, s1_fire, s2_fire, s3_fire = WireInit(false.B)
  //由于s1阶段可能由于ftq full导致预测信息无法写入，此时需要去replay pc和ghist
  val s1_replay          = WireInit(false.B)
  
  val s1_ready, s2_ready, s3_ready = WireInit(false.B)

  // val s1_target         = Wire(UInt(vaddrBitsExtended.W))








  microBtb.io.f0_valid := bpdF0Valid
  microBtb.io.f0_pc    := bpdF0Pc

  val s1_prediction = microBtb.io.resp
  val s1_phrMeta = phr.io.phrMeta
  val s1_pc             = RegEnable(s0_pc,s0_fire)
  val s1_clear          = WireInit(false.B)
  val s1_valid          = RegInit(false.B)

  val s1_rasState       = RegEnable(s0_rasState,s0_fire)
 

  val s1_finalPrediction = WireInit(s1_prediction)
  val bpuTrain = Wire(Valid(new BpuTrain))
  bpuTrain.valid := io.fromFtq.train.valid
  bpuTrain.bits.startPc := io.fromFtq.train.bits.startPc
  bpuTrain.bits.phr := phr.io.train_phr
  bpuTrain.bits.cfi := io.fromFtq.train.bits.cfi
  bpuTrain.bits.brMask := io.fromFtq.train.bits.brMask
  bpuTrain.bits.btbMeta := io.fromFtq.train.bits.btbMeta
  bpuTrain.bits.tageMeta := io.fromFtq.train.bits.tageMeta

  s1_finalPrediction.bits.target  := Mux(s1_prediction.valid, s1_prediction.bits.target, nextFetch(s1_pc))
  btb.io.f0_valid := bpdF0Valid
  btb.io.f0_pc    := bpdF0Pc
  btb.io.train    := bpuTrain

  tage.io.s0_valid := bpdF0Valid
  tage.io.s0_pc    := bpdF0Pc
  tage.io.s0_ghist := s0_phr(globalHistoryLength-1,0)
  tage.io.train    := bpuTrain
  tage.io.fromBtb.results := btb.io.resp
  tage.io.fromBtb.s1_position := btb.io.s1_position

  val s2_s1_prediction  = RegEnable(s1_finalPrediction,s1_fire)
  val s2_phrMeta         = RegEnable(s1_phrMeta,s1_fire)
  val s2_pc             = RegEnable(s1_pc,s1_fire)
  val s2_alignPc        = fetchAlign(s2_pc)
  val s2_valid          = RegInit(false.B)
  val s2_redirect       = WireInit(false.B)
  val s2_clear          = WireInit(false.B)


  val s2_newRasStateHold = RegInit((0.U).asTypeOf(new RasState))
  val s2_newRasStateHoldValid = RegInit(false.B)
  val s2_newRasState = Wire(new RasState)
  val s2_rasState       = RegEnable(s2_newRasState,s1_fire)
  val s2_tageResp = tage.io.tage_pred
  val s2_btbResp  = btb.io.resp
  val s2_jumpTakenVec = s2_btbResp.map{ btb=>
    btb.valid && ( btb.bits.attribute.isDirect || btb.bits.attribute.isIndirect)
  }

  val s2_brVec = s2_btbResp.map{ btb=>
    btb.valid &&  btb.bits.attribute.isConditional
  }
  val s2_takenMask = WireInit(VecInit((s2_tageResp.taken).zip(s2_btbResp).zipWithIndex map { case ((tage, btb), i) =>
    val useTage = tage.valid

    s2_jumpTakenVec(i) ||
    s2_brVec(i) && 
    MuxCase(
    btb.bits.taken, //direct's ctr(1) is 1
    Seq(
      useTage -> tage.bits
    )
    )
  }))
  val s2_compareMatrix = CompareMatrix(VecInit(s2_btbResp.map(_.bits.cfiPosition)))
  val s2_taken = s2_takenMask.reduce(_||_)
  val s2_firstTakenBranchOH = s2_compareMatrix.getLeastElementOH(s2_takenMask)
  val s2_firstTakenBranch = Mux1H(s2_firstTakenBranchOH, s2_btbResp)
  /*
  1. taken的分支是条件分支
  2. taken的分支是非条件分支
  3. 没有taken分支

  */
  val s2_btbHitNum = PopCount(s2_btbResp.map{btb=>
    btb.valid && btb.bits.attribute.isConditional &&
    (s2_firstTakenBranch.valid&& ((btb.bits.cfiPosition <=s2_firstTakenBranch.bits.cfiPosition) &&
     s2_firstTakenBranch.bits.attribute.isConditional || (btb.bits.cfiPosition < s2_firstTakenBranch.bits.cfiPosition) &&
     !s2_firstTakenBranch.bits.attribute.isConditional) ||
    !s2_taken)
  })

  val s2_useRas = s2_firstTakenBranch.valid && s2_firstTakenBranch.bits.attribute.isReturn && useRAS.B
  val s2_isCallOrRet = s2_firstTakenBranch.valid && (s2_firstTakenBranch.bits.attribute.isCall || s2_firstTakenBranch.bits.attribute.isReturn)
  val s2_target = MuxCase(nextFetch(s2_pc),
    Seq(
      s2_useRas -> ras.io.read_addr,
      s2_firstTakenBranch.valid -> s2_firstTakenBranch.bits.target
    )
  )

  val s2_predRasState        = WireInit(s2_rasState)
  when(s2_useRas){
    s2_predRasState.idx := WrapDec(s2_rasState.idx, nRasEntries)
  }.elsewhen(s2_firstTakenBranch.valid && s2_firstTakenBranch.bits.attribute.isCall){
    s2_predRasState.idx := WrapInc(s2_rasState.idx, nRasEntries)
  }
  s2_predRasState.top       := ras.io.read_addr

  val s2_prediction = Wire(new Prediction)
  s2_prediction.taken       := s2_taken
  s2_prediction.cfiPosition := s2_firstTakenBranch.bits.cfiPosition
  s2_prediction.target      := s2_target
  s2_prediction.attribute   := s2_firstTakenBranch.bits.attribute
  val bpdMeta = Wire(new BranchPredictionMeta)
  bpdMeta.tage     := tage.io.meta
  bpdMeta.btb      := btb.io.meta

  microBtb.io.train.valid := s2_valid && s2_redirect
  microBtb.io.train.bits.startPc := s2_pc
  microBtb.io.train.bits.branch.target := s2_prediction.target
  microBtb.io.train.bits.branch.taken := s2_prediction.taken
  microBtb.io.train.bits.branch.cfiPosition := s2_prediction.cfiPosition
  microBtb.io.train.bits.branch.attribute := s2_prediction.attribute
  microBtb.io.train.bits.branch.mispredict := false.B

  //TODO:delete handshake signal
  s0_fire               := s0_valid && s1_ready
  s1_fire               := s1_valid && s2_ready && io.toFtq.resp.ready
  s2_fire               := s2_valid 
// s1阶段ready的条件：s1阶段fire，或者s1阶段无效，或者s1阶段clear（此时s1阶段数据无效）
  s1_ready              := s1_fire || !s1_valid || s1_clear
  s2_ready              := s2_fire || !s2_valid || s2_clear

  
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


  s1_replay := s1_valid && !(s2_ready && io.toFtq.resp.ready)





  val s1_previous_pred_info    = RegNext(io.toFtq.resp.bits.f1)
  // Keep a repaired RAS state until the stalled s1 packet can advance.  The
  // repair condition is only a pulse, while s1_valid may remain asserted.
  val s2_rasRepair = s2_redirect

  val redirect_ras_repair = io.fromFtq.ras_updateInfo.valid
  val redirect_rasState = io.fromFtq.ras_updateInfo.bits.rasState
  val redirect_ras_attribute = io.fromFtq.ras_updateInfo.bits.attribute
  val redirect_ras_cfiPosition = io.fromFtq.ras_updateInfo.bits.cfiPosition
  val redirect_ras_startPc = io.fromFtq.ras_updateInfo.bits.startPc
  val redirect_ras_newState = WireInit(redirect_rasState)
  redirect_ras_newState.idx := Mux(redirect_ras_attribute.isCall,WrapInc(redirect_rasState.idx, nRasEntries),WrapDec(redirect_rasState.idx, nRasEntries))


  when (!s1_valid || s1_fire) {
    s2_newRasStateHoldValid := false.B
  }
  when (s2_rasRepair && s1_valid && !s1_fire) {
    s2_newRasStateHold      := s2_predRasState
    s2_newRasStateHoldValid := true.B
  }
  s2_newRasState := Mux(
    s2_rasRepair,
    s2_predRasState,
    Mux(s2_newRasStateHoldValid, s2_newRasStateHold, s1_rasState))

  dontTouch(s1_replay)
  s0_pc                 := MuxCase(io.reset_vector,
  Seq(
    (RegNext(reset.asBool) && !reset.asBool) -> io.reset_vector,
    io.fromFtq.redirect_val                  -> io.fromFtq.redirect_pc,
    (s2_redirect)     -> io.toFtq.resp.bits.f2.pred.target,
    s1_replay                                -> s1_pc,
    s1_valid           -> s1_finalPrediction.bits.target
  )
  )
  s0_valid              := (RegNext(reset.asBool) && !reset.asBool) || io.fromFtq.redirect_val ||
                           s2_redirect || s1_valid || s1_replay 

  s0_rasState           := MuxCase((0.U).asTypeOf(new RasState),
  Seq(
    (RegNext(reset.asBool) && !reset.asBool) -> (0.U).asTypeOf(new RasState),
    io.fromFtq.ras_updateInfo.valid        -> redirect_ras_newState,
    (s2_isCallOrRet)       -> s2_predRasState
  )) 
  //redirect and clear
  //其实这里还需要去做位置判断
  s2_redirect                 := io.toFtq.resp.bits.f2.valid&&(s1_previous_pred_info.valid)&&(s1_previous_pred_info.pred.target=/=io.toFtq.resp.bits.f2.pred.target)&&s2_valid
 
  s1_clear                    := io.fromFtq.redirect_val || (s2_redirect) 
  s2_clear                    := io.fromFtq.redirect_val

  io.toFtq.resp.bits.f1.hasRedirect := false.B
  io.toFtq.resp.bits.f2.hasRedirect := (s2_redirect) && (! s2_clear)
  //TODO:fix RAS
  ras.io.read_idx    := s1_rasState.idx
  ras.io.write_valid := s2_valid && useRAS.B && s2_taken && s2_firstTakenBranch.bits.attribute.isCall || redirect_ras_repair
  ras.io.write_addr  := Mux(redirect_ras_repair && redirect_ras_attribute.isCall,redirect_ras_startPc + (redirect_ras_cfiPosition<<1.U) + 2.U,
      Mux(redirect_ras_repair && redirect_ras_attribute.isReturn,redirect_rasState.top,s2_alignPc + (s2_prediction.cfiPosition<<1.U) + 2.U))
  ras.io.write_idx   := Mux(redirect_ras_repair, redirect_rasState.idx, s2_rasState.idx)


  io.toFtq.resp.bits.f1.pred    := s1_finalPrediction.bits
  io.toFtq.resp.bits.f1.rasState := s1_rasState


  io.toFtq.resp.bits.f2.rasState := s2_rasState
  io.toFtq.resp.bits.f2.pred    := s2_prediction
  io.toFtq.resp.bits.f2.meta     := bpdMeta


  io.toFtq.resp.bits.f1.pc := RegNext(s0_pc)
  io.toFtq.resp.bits.f2.pc := RegNext(s1_pc)

  io.toFtq.resp.valid         := s1_fire && (!s1_clear) 
  io.toFtq.resp.bits.f1.valid := s1_fire && (!s1_clear) 
  io.toFtq.resp.bits.f1.ftq_idx:= DontCare
  io.toFtq.resp.bits.f1.phrMeta := s1_phrMeta
  
  io.toFtq.resp.bits.f2.valid   := s2_fire && (!s2_clear)
  io.toFtq.resp.bits.f2.ftq_idx := RegNext(io.fromFtq.ftq_idx)
  io.toFtq.resp.bits.f2.phrMeta := s2_phrMeta


  phr.io.s0_fire := s0_fire
  phr.io.s1_fire := s1_fire
  phr.io.s2_fire := s2_fire

  phr.io.s1Train.valid := s1_fire
  phr.io.s1Train.cfiPc := fetchAlign(s1_pc) + (s1_finalPrediction.bits.cfiPosition << 1.U)
  phr.io.s1Train.taken := s1_finalPrediction.valid
  phr.io.s1Train.target := s1_finalPrediction.bits.target


  phr.io.train.s2.valid := s2_fire && s2_redirect
  phr.io.train.s2.cifPc := fetchAlign(s2_pc) + (s2_prediction.cfiPosition << 1.U)
  phr.io.train.s2.phrMeta := s2_phrMeta
  phr.io.train.s2.prediction := s2_prediction

  phr.io.train.redirect.valid := io.fromFtq.redirect.valid
  phr.io.train.redirect.bits := io.fromFtq.redirect.bits

  phr.io.bpuTrain.valid := io.fromFtq.train.valid
  phr.io.bpuTrain.bits.bpuTrainPhrMeta := io.fromFtq.train.bits.phrMeta
  phr.io.bpuTrain.bits.bpuTraincfi := io.fromFtq.train.bits.cfi


  // f2 can override f1 either because its predicted next PC differs or to
  // repair speculative history. For a target redirect, classify the f1/f2
  // disagreement by direction, terminal CFI position, then target-only.
  val bpu_override = s2_redirect 
  val bpu_override_direction = s2_redirect &&
    (s1_previous_pred_info.pred.taken =/= s2_prediction.taken)
  val bpu_override_position = s2_redirect && !bpu_override_direction &&
    (s1_previous_pred_info.pred.cfiPosition =/= s2_prediction.cfiPosition)
  val bpu_override_target = s2_redirect && !bpu_override_direction &&
    !bpu_override_position

  // BPU pipeline and training observability. These counters are deliberately
  // kept at the predictor boundary so they can be compared with the core
  // mispredict/FTQ counters without changing prediction behavior.
  XSPerfAccumulate("bpu_req", bpdF0Valid)
  XSPerfAccumulate("bpu_resp", io.toFtq.resp.valid)
  XSPerfAccumulate("bpu_s1_replay", s1_replay)
  XSPerfAccumulate("bpu_s2_redirect", s2_redirect)
  XSPerfSeqAccumulate("bpu_override", bpu_override, Seq(
    ("total", true.B),
    ("effective", !s2_clear),
    ("target_redirect", s2_redirect),
    ("direction", bpu_override_direction),
    ("position", bpu_override_position),
    ("target_only_change", bpu_override_target)
  ))
  XSPerfAccumulate("bpu_train", io.fromFtq.train.valid)
  XSPerfAccumulate("bpu_train_cfi", io.fromFtq.train.valid && io.fromFtq.train.bits.cfi.valid)
  XSPerfAccumulate("bpu_train_cond", io.fromFtq.train.valid &&
    io.fromFtq.train.bits.cfi.valid && io.fromFtq.train.bits.cfi.attribute.isConditional)
  XSPerfAccumulate("bpu_train_mispred", io.fromFtq.train.valid &&
    io.fromFtq.train.bits.cfi.valid && io.fromFtq.train.bits.cfi.mispredict)

  val predictionTraceRecord = Wire(new BpuPredictionTrace)
  predictionTraceRecord.startPc := s2_pc
  predictionTraceRecord.f1Taken := s2_s1_prediction.bits.taken
  predictionTraceRecord.f1Position := s2_s1_prediction.bits.cfiPosition
  predictionTraceRecord.f2Taken := s2_prediction.taken
  predictionTraceRecord.f2Position := s2_prediction.cfiPosition
  predictionTraceRecord.f2Target := s2_prediction.target
  predictionTraceRecord.s2Redirect := s2_redirect
  predictionTraceRecord.ghistRepair := 0.U
  predictionTrace.log(predictionTraceRecord, s2_fire && !s2_clear, "bpu-f2")(clock, reset)

  val trainTraceRecord = Wire(new BpuTrainTrace)
  trainTraceRecord.startPc := io.fromFtq.train.bits.startPc
  trainTraceRecord.cfiValid := io.fromFtq.train.bits.cfi.valid
  trainTraceRecord.cfiPosition := io.fromFtq.train.bits.cfi.position
  trainTraceRecord.actualTaken := io.fromFtq.train.bits.cfi.taken
  trainTraceRecord.mispredict := io.fromFtq.train.bits.cfi.mispredict
  trainTraceRecord.attribute := io.fromFtq.train.bits.cfi.attribute.asUInt
  trainTrace.log(trainTraceRecord, io.fromFtq.train.valid, "ftq-train")(clock, reset)

  // We don't care about meta from the f1 and f2 resps
  // Use the meta from the latest resp
  io.toFtq.resp.bits.f1.meta  := DontCare
/* --------------------------------- update --------------------------------- */

}
