package boom.fdip.ifu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket.{TLBReq, TLBResp}
import boom.fdip.common._
import boom.fdip.util._

/*
1. s0阶段先从waylookup读出metadata，发出读数据请求
2. s1阶段接收数据，并查看数据是否miss
3. 如果miss发送miss请求，否则发送数据

TODO: 加入跨行取指
*/
class MainPipe(implicit p: Parameters) extends ICacheModule{
    val io = IO(new Bundle{
        val flush = Input(Bool())
        val fromFtq = Flipped(DecoupledIO(new FTQToMainPipeIO))
        val toIFU = DecoupledIO(new MainPipeDataResp)
        val wayLookupToMainPipe = Flipped(DecoupledIO(new WayLookupToMainPipe))

        val mainPipeDataReq = DecoupledIO(new MainPipeDataReq)
        val mainPipeDataResp = Flipped(ValidIO(new MainPipeDataResp))

        val mainPipeMissReq = DecoupledIO(new ICacheMissReq)
        val mainPipeMissResp = Flipped(ValidIO(new ICacheMissResp))
    })

    val s0_valid = io.fromFtq.valid && !io.flush && io.wayLookupToMainPipe.valid
    val s0_fire,s1_fire = Wire(Bool())
    val s0_ftqReq = WireInit(io.fromFtq.bits)
    val s0_wayLookupEntry = WireInit(io.wayLookupToMainPipe.bits.entry)
    val s0_wayMask = s0_wayLookupEntry.waymask
    val s0_vSetIdx = s0_wayLookupEntry.vSetIdx
    val s0_pTag = s0_wayLookupEntry.pTag

    val s1_ready = Wire(Bool())

    val s0_canIssue = io.mainPipeDataReq.ready &&s1_ready 

    io.fromFtq.ready := io.fromFtq.valid && !io.flush && s0_canIssue
    io.wayLookupToMainPipe.ready := io.wayLookupToMainPipe.valid && !io.flush && s0_canIssue

    s0_fire := s0_valid && s0_canIssue

    io.mainPipeDataReq.valid := s0_fire
    io.mainPipeDataReq.bits.vSetIdx := s0_vSetIdx
    io.mainPipeDataReq.bits.way := PriorityEncoder(s0_wayMask)

    val s1_wayMask = RegEnable(s0_wayMask,s0_fire)
    val s1_vSetIdx = RegEnable(s0_vSetIdx,s0_fire)
    val s1_pTag = RegEnable(s0_pTag,s0_fire)
    val s1_valid = RegInit(false.B)
    val s1_sramValid = RegNext(s0_fire)
    when(io.flush){
        s1_valid := false.B
    }.elsewhen(s0_fire){
        s1_valid := true.B
    }.elsewhen(s1_fire){
        s1_valid := false.B
    }

    val s1_hit = s1_wayMask.orR
    val s1_dataResp = WireInit(io.mainPipeDataResp.bits)
    val s1_waitMissResp = RegInit(false.B)
    val s1_missResp = WireInit(io.mainPipeMissResp.bits)
    val s1_missDataResp = Wire(new MainPipeDataResp)
    s1_missDataResp.data := s1_missResp.data
    val s1_missRespValid = WireInit(io.mainPipeMissResp.valid)
    val s1_missRespHit = s1_missRespValid && s1_missResp.blkPaddr >> blockOffBits.U === s1_pTag && s1_missResp.vSetIdx === s1_vSetIdx
    
    when(io.flush){
        s1_waitMissResp := false.B
    }.elsewhen(s1_valid && !s1_hit){
        s1_waitMissResp := true.B
    }.elsewhen(io.mainPipeMissResp.valid){
        s1_waitMissResp := false.B
    }

    val s1_finalHit = Mux(s1_missRespHit||s1_sramValid,s1_missRespHit || s1_hit,RegEnable(s1_missRespHit || s1_hit, s1_missRespHit||s1_sramValid))
    val s1_dataHold = RegEnable(
        Mux(s1_missRespHit, s1_missDataResp, s1_dataResp),
        s1_missRespHit || s1_sramValid)
    val s1_finalData = Mux(s1_missRespHit, s1_missDataResp,
        Mux(s1_sramValid, s1_dataResp, s1_dataHold))

    val s1_fetchFinish = !s1_waitMissResp || s1_missRespHit
    // 取指令完成或者!s1_valid
    s1_ready := s1_fetchFinish && io.toIFU.ready || !s1_valid



    io.mainPipeMissReq.valid := s1_valid && !s1_finalHit && !s1_waitMissResp
    io.mainPipeMissReq.bits.blkPaddr := Cat(s1_pTag,s1_vSetIdx,0.U(blockOffBits.W))
    io.mainPipeMissReq.bits.way := PriorityEncoder(s1_wayMask)
    io.mainPipeMissReq.bits.vSetIdx := s1_vSetIdx


    io.toIFU.valid := s1_valid && s1_fetchFinish && s1_finalHit
    io.toIFU.bits := s1_finalData

    s1_fire := s1_valid && s1_fetchFinish && io.toIFU.ready
}
