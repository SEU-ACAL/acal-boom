//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Fetch Target Queue (FTQ)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Each entry in the FTQ holds the fetch address and branch prediction snapshot state.
//
// TODO:
// * reduce port counts.

package boom.fdip.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.util.{Str}
import boom.fdip.ifu._
import boom.fdip.common._
import boom.fdip.util._
import boom.fdip.exu._
import boom.fdip.util._
import boom.fdip.ifu.bpd._
 import boom.fdip.ifu.bpd.ras.RasState
import freechips.rocketchip.tilelink.TLMessages.isA
import os.write.over
import freechips.rocketchip.regmapper.RRTest0Map.re

/**
 * FTQ Parameters used in configurations
 *
 * @param nEntries # of entries in the FTQ
 */
case class FtqParameters(
  nEntries: Int = 16
)

/**
 * Bundle to add to the FTQ RAM and to be used as the pass in IO
 */

class FTQBundle(implicit p: Parameters) extends BoomBundle
  with HasBoomFrontendParameters
{
  // // TODO compress out high-order bits
  // val fetch_pc  = UInt(vaddrBitsExtended.W)
  // IDX of instruction that was predicted taken, if any
  val cfiPosition = Valid(UInt(log2Ceil(predWidth).W))
  // Was the CFI in this bundle found to be taken? or not
  val cfi_taken = Bool()
  // Was this CFI mispredicted by the branch prediction pipeline?
  val cfi_mispredicted = Bool()
  // What type of CFI was taken out of this bundle
  val attribute = new BranchAttribute
  // mask of branches which were visible in this fetch bundle
  val br_mask   = UInt(predWidth.W)
}

class FTQPCBundle(implicit p: Parameters) extends BoomBundle
  with HasBoomFrontendParameters
{

  // TODO: check mask 
  val start_pc       = UInt(vaddrBitsExtended.W)
  val mask           = UInt(fetchWidth.W)//得到end_pc
  val exit_pc_offset = UInt((fetchBytes/2).W)//以C扩展为基准
  val is_exit_branch = Bool()
  val is_exit_taken  = Bool()
  val target         = UInt(vaddrBitsExtended.W)//可以优化
}
class FtqToICacheRequestBundle(implicit p: Parameters) extends BoomBundle
  with HasBoomFrontendParameters
{
  // // TODO compress out high-order bits
  val fetch_pc  = UInt(vaddrBitsExtended.W)
  val mask      = UInt(fetchWidth.W)
}
class FtqRequestBundle(implicit p: Parameters) extends BoomBundle
  with HasBoomFrontendParameters
{
  // // TODO compress out high-order bits
  val ftq_entry  = (new FTQPCBundle)
  val ftq_offset = Valid(UInt((log2Ceil(fetchWidth)).W))
  val ftq_idx    = (UInt((log2Ceil(ftqSz)+1).W))
  // val target     = UInt(vaddrBitsExtended.W)
}
class IFURequestBundle(implicit p: Parameters) extends BoomBundle
  with HasBoomFrontendParameters
{
  // // TODO compress out high-order bits
  val pfc_info   = (new FTQBundle)
  val ftq_idx    = (UInt((log2Ceil(ftqSz)+1).W))
  val cfi_idx    = Valid(UInt(log2Ceil(fetchWidth).W))
}
class SpecInfoBundle(implicit p: Parameters) extends BoomBundle
  with HasBoomFrontendParameters
{
  // // TODO compress out high-order bits
  val ghist   = new GlobalHistory
  val rasState = new RasState
}
/**
 * IO to provide a port for a FunctionalUnit to get the PC of an instruction.
 * And for JALRs, the PC of the next instruction.
 */
class GetPCFromFtqIO(implicit p: Parameters) extends BoomBundle
{
  val ftq_idx   = Input(UInt((log2Ceil(ftqSz)+1).W))

  val entry     = Output(new FTQBundle)
  val ghist     = Output(new GlobalHistory)

  val pc        = Output(UInt(vaddrBitsExtended.W))
  val com_pc    = Output(UInt(vaddrBitsExtended.W))

  // the next_pc may not be valid (stalled or still being fetched)
  val next_val  = Output(Bool())
  val next_pc   = Output(UInt(vaddrBitsExtended.W))
}

class CTRLToFTQIO(implicit p: Parameters) extends BoomBundle
{
  val redirect_val    = Output(Bool())
  // val redirect_target
  val redirect_pc     = Output(UInt(vaddrBitsExtended.W))
  val redirect_ftq_idx= Output(UInt((log2Ceil(ftqSz)+1).W)) 
  // val redirect_ghist  = Output(new GlobalHistory)

  val deq             = Valid(UInt((log2Ceil(ftqSz)+1).W))
  val brupdate        = Output(new BrUpdateInfo)
}

class FTQToCTRLIO(implicit p: Parameters) extends BoomBundle
{
  val get_ftq_pc = Vec(2, new GetPCFromFtqIO())
}

class FTQToICacheIO(implicit p: Parameters) extends BoomBundle
{
  val req = DecoupledIO(new FtqToICacheRequestBundle)
}

class FTQToBPUIO(implicit p: Parameters) extends BoomBundle
{
  val ftq_idx   = Output(UInt((log2Ceil(ftqSz)+1).W))

  // val ftq_full  = Output(Bool())
  val redirect_val    = Output(Bool())
  val redirect_pc     = Output(UInt(vaddrBitsExtended.W))
  val redirect_ftq_idx= Output(UInt((log2Ceil(ftqSz)+1).W)) 
  val redirect_ghist  = Output(new GlobalHistory)
  // val redirect_rasState = Output(new RasState)

  val redirect_btb_repair = Output(Bool())

  val train = Output(Valid(new Train))
  // val bpdupdate = Output(Valid(new BranchPredictionUpdate))


  val ras_updateInfo = Valid(new RasUpdateInfo())
}

class FTQToPrefetchIO(implicit p: Parameters) extends BoomBundle
{
  val fetch_pc  = UInt(vaddrBitsExtended.W)
  val mask      = UInt(fetchWidth.W)
}


class BPUFlushInfo(implicit p: Parameters) extends BoomBundle
{
  val s2_redirect = Valid(UInt((log2Ceil(ftqSz)+1).W))
  val s3_redirect = Valid(UInt((log2Ceil(ftqSz)+1).W))
  def flushByS2(idx:UInt): Bool = {
    return s2_redirect.valid && (s2_redirect.bits === idx)
  }
  def flushByS3(idx:UInt): Bool = {
    return s3_redirect.valid && (s3_redirect.bits === idx)
  }
}
class FTQToIfuIO(implicit p: Parameters) extends BoomBundle
{
  val req         = DecoupledIO(new FtqRequestBundle)
  val flushByBpu  = new BPUFlushInfo
  val redirect    = Valid(UInt((log2Ceil(ftqSz)+1).W))
  //TODO:ADD signals

}
// class IfuToFTQIO(implicit p: Parameters) extends BoomBundle
// {
//   //TODO:ADD signals
//   val pd_info = DecoupledIO(new IFURequestBundle)
// }

/**
 * Queue to store the fetch PC and other relevant branch predictor signals that are inflight in the
 * processor.
 *
 * @param num_entries # of entries in the FTQ
 */

/*

 */
class FDIPFetchTargetQueue(implicit p: Parameters) extends BoomModule
  with HasBoomCoreParameters
  with HasBoomFrontendParameters
{
  // override val ftqSz: Int = boomParams.FDIPftq.nEntries
  assert(isPow2(ftqSz) && ftqSz >= 2)//now it onlty support pow2,
  val num_entries = ftqSz
  private val ftqIdxBits = log2Ceil(num_entries)
  private val ftqPtrBits = ftqIdxBits + 1
  val io = IO(new BoomBundle {
    val fromBpu     = Flipped(new BPUToFTQIO)
    val fromBackend = Flipped(new CTRLToFTQIO)
    val fromIfu     = Flipped(new IFUtoFTQ)

    val toBackend   = new FTQToCTRLIO
    val toBPU       = new FTQToBPUIO
    val toICache    = new FTQToICacheIO
    val toPrefetch  = DecoupledIO(new FTQToPrefetchIO)
    val toIfu       = new FTQToIfuIO
    // Used to regenerate PC for trace port stuff in FireSim
    // Don't tape this out, this blows up the FTQ
    val ftq_redirect_conflict = Output(Bool())
    val ftq_meta_conflict     = Output(Bool())
    val ftq_full              = Output(Bool())
    val debug_ftq_idx  = Input(Vec(coreWidth, UInt((log2Ceil(ftqSz)+1).W)))
    val debug_fetch_pc = Output(Vec(coreWidth, UInt(vaddrBitsExtended.W)))
  })
  // FTQ pointers carry a wrap bit; storage arrays only consume the entry index.
  def ftqMemIdx(ptr: UInt): UInt = {
    ptr(ftqIdxBits - 1, 0)
  }
  def ftqWrap(ptr: UInt): Bool = {
    ptr(ftqIdxBits)
  }
  def isAfter(idx1:UInt,idx2:UInt): Bool = {//越小越好
    val flag1 = ftqWrap(idx1)
    val flag2 = ftqWrap(idx2)
    val ptr1  = ftqMemIdx(idx1)
    val ptr2  = ftqMemIdx(idx2)
    return Mux(flag1 === flag2, ptr1 < ptr2, ptr1 >ptr2)
  }
  def isBefore(idx1:UInt,idx2:UInt): Bool = {//这个得出谁更老
    val flag1 = ftqWrap(idx1)
    val flag2 = ftqWrap(idx2)
    val ptr1  = ftqMemIdx(idx1)
    val ptr2  = ftqMemIdx(idx2)
    return Mux(flag1 === flag2, ptr1 > ptr2, ptr1 < ptr2)
  }
  def isFull(idx1:UInt,idx2:UInt): Bool = {//越小越好
    val flag1 = ftqWrap(idx1)
    val flag2 = ftqWrap(idx2)
    val ptr1  = ftqMemIdx(idx1)
    val ptr2  = ftqMemIdx(idx2)
    return flag1 =/= flag2 && ptr1 === ptr2
  }
  def isEmpty(idx1:UInt,idx2:UInt): Bool = {//越小越好
    val flag1 = ftqWrap(idx1)
    val flag2 = ftqWrap(idx2)
    val ptr1  = ftqMemIdx(idx1)
    val ptr2  = ftqMemIdx(idx2)
    return flag1 === flag2 && ptr1 === ptr2
  }
  def CirWrapInc(ptr:UInt): UInt = {//越小越好
    Mux(ftqMemIdx(ptr) === (num_entries - 1).U, Cat(~ftqWrap(ptr), 0.U(ftqIdxBits.W)), ptr + 1.U)
  }


  val pc_mem        = Reg(Vec(num_entries, new FTQPCBundle))//后端读pc

  val meta          = Reg(Vec(num_entries, new BranchPredictionMeta))//commit update

  
  val redirect_mem  = Reg(Vec(num_entries, new SpecInfoBundle()))//some info needed to redirect:IFU-redirect+backend-redirect+commit
  val pd_mem        = Reg(Vec(num_entries, new FTQBundle))//some info needed to redirect:IFU-redirect+backend-redirect+commit
  
  val cfi_offset    = Reg(Vec(num_entries, Valid(UInt((log2Ceil(predWidth)).W))))
  // val ghist         = Seq.fill(2) { SRAMHelper(num_entries, new GlobalHistory) }

  //目前对这些指针的管理仍然高度依赖于位置，位置越靠前，优先级越低，之后考虑直接使用mux来做选择
  val bpu_ptr    = RegInit(0.U(ftqPtrBits.W))
  val ifu_ptr    = RegInit(0.U(ftqPtrBits.W))
  val pf_ptr     = RegInit(0.U(ftqPtrBits.W))
  val commit_ptr = RegInit(0.U(ftqPtrBits.W))//后端提交指针
  val deq_ptr    = RegInit(0.U(ftqPtrBits.W))//后端此次提交最年轻指令的ftq_idx
  val full       = isFull(commit_ptr,bpu_ptr)


  /* -------------------------------------------------------------------------- */
  /*                                   From Bpu                                 */
  /* -------------------------------------------------------------------------- */
  io.fromBpu.resp.ready := (!full)


  // bpu info process
  val f1_resp    = io.fromBpu.resp.bits.f1
  val f1_mask    = fetchMask(f1_resp.pc)
  

  //s2,s3阶段的重定向写入的优先级更高
  val bpu_s2_redirect = WireInit(io.fromBpu.resp.bits.f2.hasRedirect)
  //PC ENQ
  val bpu_in_resp    = io.fromBpu.resp.bits.selectedResp
  val bpu_in_stage   = io.fromBpu.resp.bits.selectedRespIdxForFtq
  val bpu_in_resp_ptr= Mux(bpu_in_stage===BP_S1,bpu_ptr,bpu_in_resp.ftq_idx)
  val bpu_in_entry   = io.fromBpu.resp.bits.selectedFtqentry
  val bpu_in_wen     = (io.fromBpu.resp.fire || bpu_s2_redirect) && 
                      !(io.fromBackend.redirect_val||io.fromIfu.redirect_from_fetch)//后端重定向时不写入

  val bpu_last_stage_resp = WireInit(io.fromBpu.resp.bits.lastStage)
  val bpu_last_stage_ptr  = WireInit(io.fromBpu.resp.bits.lastStage.ftq_idx)
  val bpu_last_stage_meta = WireInit(io.fromBpu.resp.bits.lastStage.meta)
  val bpu_last_stage_ghist= WireInit(io.fromBpu.resp.bits.lastStage.ghist)
  val bpu_last_stage_wen  = WireInit(io.fromBpu.resp.bits.lastStage.valid && 
                            !(io.fromBackend.redirect_val||io.fromIfu.redirect_from_fetch))
  val bpu_last_stage_ras_top = io.fromBpu.resp.bits.lastStage.rasState.top
  dontTouch(bpu_s2_redirect)
  // dontTouch(bpu_s3_redirect)
  dontTouch(bpu_last_stage_wen)
  dontTouch(bpu_last_stage_ptr)
  dontTouch(bpu_last_stage_meta)
  dontTouch(bpu_last_stage_ghist)
  dontTouch(bpu_last_stage_resp)
  // meta.io.w.wen     := bpu_last_stage_wen
  // meta.io.w.waddr   := ftqMemIdx(bpu_last_stage_ptr)
  // meta.io.w.wdata   := bpu_last_stage_meta


  when(bpu_last_stage_wen){
    meta(ftqMemIdx(bpu_last_stage_ptr)) := bpu_last_stage_meta
    val redirect_info = Wire(new SpecInfoBundle())
    redirect_info.ghist := bpu_last_stage_ghist
    redirect_info.rasState.top := bpu_last_stage_ras_top
    redirect_info.rasState.idx := bpu_last_stage_resp.rasState.idx
    redirect_mem(ftqMemIdx(bpu_last_stage_ptr)) := redirect_info
  }
  //默认读优先，但如果重定向的话，写入的一定是无效信息，所以可以禁止写入
  /* 
  这里发生读写冲突，会导致更新BPU晚一个周期，对整体的性能影响有待评估
   */
  io.ftq_redirect_conflict := false.B         
  io.ftq_meta_conflict     := false.B
  io.ftq_full              := full


  when(bpu_in_wen){
    pc_mem(ftqMemIdx(bpu_in_resp_ptr)) := bpu_in_entry
    cfi_offset(ftqMemIdx(bpu_in_resp_ptr)).valid := !bpu_in_resp.pred.attribute.isNone && bpu_in_resp.pred.taken 
    cfi_offset(ftqMemIdx(bpu_in_resp_ptr)).bits := bpu_in_resp.pred.cfiPosition
  }
  


  //BPU other mem write
  /* 
  update :后端异常+分支预测失败+正常commit
  后端异常：直接刷新流水
  分支预测失败：此时不仅需要恢复预测失败的ghist,
  正常提交：此时会更新commit指针，但由于前端一个fetch block可能对应多个后端的block，所以commit_ptr指针更新策略
  此时弊端commit_ptr指针略过update信息，导致update失败
   */
/* -------------------------------------------------------------------------- */
/*                                  From IFU                                  */
/* -------------------------------------------------------------------------- */
//本阶段送入的br_mask一定是对的，无论分支是否错误

  val new_entry = Wire(new FTQBundle)
  val prev_ghist = RegInit((0.U).asTypeOf(new GlobalHistory))
  val prev_entry = RegInit((0.U).asTypeOf(new FTQBundle))
  new_entry.cfiPosition           := io.fromIfu.predecode_infos.bits.br_infos.cfiPosition
  new_entry.cfi_taken         := io.fromIfu.predecode_infos.bits.br_infos.cfi_taken
  new_entry.cfi_mispredicted  :=     io.fromIfu.redirect_from_fetch && io.fromIfu.predecode_infos.bits.br_infos.cfiPosition.valid

  new_entry.attribute          := io.fromIfu.predecode_infos.bits.br_infos.attribute
  new_entry.br_mask           := io.fromIfu.predecode_infos.bits.br_infos.br_mask 
  //pfc入队只要没有full就行，这个full条件其实是pc_mem的，写入的地址来自ftq传往ifu的指针
  // io.fromIfu.predecode_infos.ready := (!full)
  when(io.fromIfu.predecode_infos.valid){
    pd_mem(ftqMemIdx(io.fromIfu.predecode_infos.bits.ftq_idx)) := new_entry
  }



  /* -------------------------------------------------------------------------- */
  /*                                 From Backend                               */
  /* -------------------------------------------------------------------------- */

  val bpd_update_mispredict = RegInit(false.B)
  val bpd_update_fetch      = RegInit(false.B)
  val bpd_update_commit     = RegInit(false.B)
  val do_update_commit      = WireInit(false.B)
  val bpd_update_ptr        = Wire(UInt(ftqPtrBits.W))
  val bpd_update_idx        = ftqMemIdx(bpd_update_ptr)
  val bpd_update_entry      = RegNext(pd_mem(bpd_update_idx))
  val bpd_entry             = WireInit(pd_mem(bpd_update_idx))
  val bpd_redirect_entry    = WireInit(pd_mem(bpd_update_idx))
  val bpd_update_pc         = RegNext(pc_mem(bpd_update_idx).start_pc)
  val bpd_update_ghist      = RegNext(redirect_mem(bpd_update_idx).ghist) //需要传入BPU修复后的
  val bpd_update_ras_top    = RegNext(redirect_mem(bpd_update_idx).rasState.top)
  val bpd_update_meta       = RegNext(meta(ftqMemIdx(commit_ptr)))
  val ifu_redirect_ghist    = WireInit(bpd_update_ghist)
  val backend_redirect_ghist = WireInit(bpd_update_ghist)
  val bpd_update_target     = RegNext(pc_mem(ftqMemIdx(CirWrapInc(bpd_update_ptr))).start_pc)

  val ras_update_info       = RegNext(Mux(io.fromBackend.redirect_val&&io.fromBackend.brupdate.b2.mispredict,pd_mem(bpd_update_idx),new_entry))
  //只有commit阶段可能遇到读写同时发生，此时阻塞读，直到写入完成
  bpd_update_ptr                     := Mux(io.fromBackend.redirect_val&&io.fromBackend.brupdate.b2.mispredict,
                            io.fromBackend.redirect_ftq_idx,Mux(io.fromIfu.redirect_from_fetch,
                            io.fromIfu.redirect_ftq_idx,commit_ptr))
  // meta.io.r.req.valid                := (commit_ptr=/=deq_ptr)
  // meta.io.r.req.bits.raddr           := ftqMemIdx(commit_ptr)

  //对于redirect_mem 读的优先级大于写，一般读是为了重定向，此时写入的一定无效
  /* 
  提交时需要去根据ghist来得到更新ctr的idx
  */
  // redirect_mem.io.r.req.bits.raddr   := bpd_update_idx
  // redirect_mem.io.r.req.valid        := io.fromBackend.redirect_val||io.fromIfu.redirect_from_fetch||(commit_ptr=/=deq_ptr)
  bpd_update_mispredict              := (io.fromBackend.redirect_val&&io.fromBackend.brupdate.b2.mispredict||io.fromIfu.redirect_from_fetch)
  //必须redirect和meta全部读出来，此时才可以更新
  do_update_commit                   := (!(io.fromBackend.redirect_val&&io.fromBackend.brupdate.b2.mispredict||io.fromIfu.redirect_from_fetch))&&
                                (commit_ptr=/=deq_ptr)&&(io.fromBackend.redirect_val||io.fromIfu.redirect_from_fetch||(commit_ptr=/=deq_ptr))
  bpd_update_commit                  := do_update_commit //第二个周期读出update信息，然后送入bpu




/* -------------------------------- fix entry ------------------------------- */
  // val br_update_entry


  when(io.fromBackend.redirect_val){

    
    when(io.fromBackend.brupdate.b2.mispredict){
      // CFI position is the terminal halfword of the instruction.  pc_lob is
      // the instruction start offset, so an RVI branch occupies the next
      // halfword position while an RVC branch uses its start position.
      // Keep the same encoding used by PreChecker and FetchBuffer; otherwise
      // a repaired RVI entry gets a false uop.taken value and its not-taken
      // resolution cannot generate a backend redirect.
      val new_cfi_idx = Mux(
        io.fromBackend.brupdate.b2.uop.is_rvc || io.fromBackend.brupdate.b2.uop.edge_inst,
        io.fromBackend.brupdate.b2.uop.pc_lob(log2Ceil(fetchWidth), 1),
        io.fromBackend.brupdate.b2.uop.pc_lob(log2Ceil(fetchWidth), 1) + 1.U)
      bpd_redirect_entry.cfiPosition.valid    := true.B
      bpd_redirect_entry.cfiPosition.bits     := new_cfi_idx
      bpd_redirect_entry.cfi_mispredicted := true.B
      bpd_redirect_entry.cfi_taken        := io.fromBackend.brupdate.b2.taken
      bpd_redirect_entry.attribute.branchType := Mux(
        io.fromBackend.brupdate.b2.cfi_type === CFI_BR,
        BranchAttribute.BranchType.Conditional,
        Mux(io.fromBackend.brupdate.b2.cfi_type === CFI_JAL,
          BranchAttribute.BranchType.Direct,
          Mux(io.fromBackend.brupdate.b2.cfi_type === CFI_JALR,
            BranchAttribute.BranchType.Indirect,
            BranchAttribute.BranchType.None)))
      bpd_redirect_entry.attribute.rasAction := Mux(
        io.fromBackend.brupdate.b2.is_call && io.fromBackend.brupdate.b2.is_ret,
        BranchAttribute.RasAction.PopAndPush,
        Mux(io.fromBackend.brupdate.b2.is_call,
          BranchAttribute.RasAction.Push,
          Mux(io.fromBackend.brupdate.b2.is_ret,
            BranchAttribute.RasAction.Pop,
            BranchAttribute.RasAction.None)))
    }.otherwise{
      /* 
      此时由于前端PFC，可能存储的为jal指令，但实际上，这个指令被前面的redirect打断，此时不能更新这个信息
       */
      bpd_redirect_entry.cfiPosition.valid    := false.B
      bpd_redirect_entry.cfi_mispredicted := false.B
    }

    pd_mem(bpd_update_idx) := bpd_redirect_entry
  }

  /* -------------------------------------------------------------------------- */
  /*                                  PTR Update                                */
  /* -------------------------------------------------------------------------- */

  pf_ptr  := MuxCase(pf_ptr,
    Seq(
      io.fromBackend.redirect_val -> CirWrapInc(io.fromBackend.redirect_ftq_idx),
      io.fromIfu.redirect_from_fetch -> CirWrapInc(io.fromIfu.redirect_ftq_idx),
     (bpu_s2_redirect && !isBefore(io.fromBpu.resp.bits.f2.ftq_idx,pf_ptr)) -> io.fromBpu.resp.bits.f2.ftq_idx ,
      (io.toPrefetch.fire&& !isAfter(pf_ptr,ifu_ptr)) -> CirWrapInc(pf_ptr)
  ))

  bpu_ptr := MuxCase(bpu_ptr,
    Seq(
      io.fromBackend.redirect_val -> CirWrapInc(io.fromBackend.redirect_ftq_idx),
      io.fromIfu.redirect_from_fetch -> CirWrapInc(io.fromIfu.redirect_ftq_idx),
      bpu_s2_redirect -> CirWrapInc(io.fromBpu.resp.bits.f2.ftq_idx),
      (bpu_in_wen && (bpu_in_stage === BP_S1)) ->CirWrapInc(bpu_in_resp_ptr)
  ))

  ifu_ptr := MuxCase(ifu_ptr,
    Seq(
      io.fromBackend.redirect_val -> CirWrapInc(io.fromBackend.redirect_ftq_idx),
      io.fromIfu.redirect_from_fetch -> CirWrapInc(io.fromIfu.redirect_ftq_idx),
      (bpu_s2_redirect && !isBefore(io.fromBpu.resp.bits.f2.ftq_idx,ifu_ptr))  -> io.fromBpu.resp.bits.f2.ftq_idx,
      io.toICache.req.fire ->  CirWrapInc(ifu_ptr)
  ))

  deq_ptr                   := Mux(io.fromBackend.deq.valid,io.fromBackend.deq.bits,deq_ptr)
  commit_ptr                := Mux(do_update_commit,CirWrapInc(commit_ptr),commit_ptr)


  /* -------------------------------------------------------------------------- */
  /*                                  To ICache                                 */
  /* -------------------------------------------------------------------------- */
  
  /* 
  送到ICache的请求需要保证ifu的指针不能跟上bpu指针
  */
  
  io.toICache.req.valid          := (!isEmpty(ifu_ptr,bpu_ptr)) && !(io.fromBackend.redirect_val||io.fromIfu.redirect_from_fetch) &&
                                  (!(bpu_s2_redirect && !isBefore(io.fromBpu.resp.bits.f2.ftq_idx,ifu_ptr)))
  io.toICache.req.bits.fetch_pc  := pc_mem(ftqMemIdx(ifu_ptr)).start_pc
  io.toICache.req.bits.mask      := pc_mem(ftqMemIdx(ifu_ptr)).mask
  //送出后ifu指针+1

  /* -------------------------------------------------------------------------- */
  /*                                   TO BPU                                   */
  /* -------------------------------------------------------------------------- */


  //we DONT USE loop predictor
  io.toBPU.ftq_idx := bpu_ptr


  val update_cfi = Wire(new ResolvedCfi)
  update_cfi.valid      := bpd_update_entry.cfiPosition.valid
  update_cfi.position   := bpd_update_entry.cfiPosition.bits
  update_cfi.taken      := bpd_update_entry.cfi_taken
  update_cfi.target     := bpd_update_target
  update_cfi.mispredict := bpd_update_entry.cfi_mispredicted
  update_cfi.attribute := bpd_update_entry.attribute

  val update_br_mask = Mux(update_cfi.valid,
    MaskLower(UIntToOH(update_cfi.position)) & bpd_update_entry.br_mask,
    bpd_update_entry.br_mask)

  io.toBPU.train.valid := bpd_update_commit && (update_cfi.valid || update_br_mask =/= 0.U)
  io.toBPU.train.bits.startPc := bpd_update_pc
  io.toBPU.train.bits.ghist   := bpd_update_ghist
  io.toBPU.train.bits.brMask  := update_br_mask
  io.toBPU.train.bits.cfi     := update_cfi
  io.toBPU.train.bits.btbMeta := bpd_update_meta.btb
  io.toBPU.train.bits.tageMeta    := bpd_update_meta.tage

  val ifu_redirect_entry  = RegNext(new_entry)
  ifu_redirect_ghist := bpd_update_ghist


  /*
  * if redirect hit btb,use backend_hitBtbNum,otherwise use backend_hitBtbNum + 1
  */
  val backend_redirectPosition = Mux(io.fromBackend.brupdate.b2.uop.is_rvc || io.fromBackend.brupdate.b2.uop.edge_inst,
    io.fromBackend.brupdate.b2.uop.pc_lob/(coreInstBytes.U),io.fromBackend.brupdate.b2.uop.pc_lob/(coreInstBytes.U) + 1.U)
  val redirect_needUpdateGhist = io.fromBackend.redirect_val && io.fromBackend.brupdate.b2.mispredict && io.fromBackend.brupdate.b2.uop.is_br
  val backend_redirect_meta = meta(ftqMemIdx(io.fromBackend.redirect_ftq_idx))
  val backend_redirectHitBtb = backend_redirect_meta.btb.map{meta=>
    meta.valid && meta.bits.position === backend_redirectPosition && io.fromBackend.redirect_val &&
    (io.fromBackend.brupdate.b2.mispredict)
  }
  //
  val backend_hitBtbNum =  backend_redirect_meta.btb.map{meta=>
    meta.valid && meta.bits.attribute.isConditional && 
    io.fromBackend.redirect_val && (io.fromBackend.brupdate.b2.mispredict) &&
    (meta.bits.position <= backend_redirectPosition && io.fromBackend.brupdate.b2.uop.is_br ||
    meta.bits.position < backend_redirectPosition && !io.fromBackend.brupdate.b2.uop.is_br)
  }

  val backendHitCountWidth = log2Ceil(backend_redirect_meta.btb.length + 2)
  val backend_finalHitNum = Wire(UInt(backendHitCountWidth.W))
  backend_finalHitNum := Mux(
    backend_redirectHitBtb.reduce(_||_),
    PopCount(backend_hitBtbNum),
    PopCount(backend_hitBtbNum) + 1.U)
  val redirect_needUpdateGhist_r = RegNext(redirect_needUpdateGhist)
  val backend_finalHitNum_r = Reg(UInt(backendHitCountWidth.W))
  backend_finalHitNum_r := backend_finalHitNum
  val backend_redirect_taken_r = RegNext(io.fromBackend.brupdate.b2.taken)
  backend_redirect_ghist := Mux(redirect_needUpdateGhist_r, bpd_update_ghist.update(backend_finalHitNum_r, backend_redirect_taken_r, redirect_needUpdateGhist_r), bpd_update_ghist)



  io.toBPU.redirect_val                          := RegNext(io.fromBackend.redirect_val || io.fromIfu.redirect_from_fetch)      
  io.toBPU.redirect_ftq_idx                      := RegNext(Mux(io.fromBackend.redirect_val,io.fromBackend.redirect_ftq_idx,io.fromIfu.redirect_ftq_idx))  
  io.toBPU.redirect_pc                           := RegNext(Mux(io.fromBackend.redirect_val,io.fromBackend.redirect_pc,io.fromIfu.redirect_target))  
  io.toBPU.redirect_ghist                        := Mux(RegNext(io.fromBackend.redirect_val),backend_redirect_ghist,ifu_redirect_ghist)
  
  //TODO: remove this, we should not repair BTB on IFU redirect
  io.toBPU.redirect_btb_repair                   := RegNext(io.fromIfu.redirect_btb)//只有ifu的重定向才会early修复btb,其他的就是commit更新
  //RAS目前只修复由于call指令导致的错误（一般是未识别导致的）
  io.toBPU.ras_updateInfo.valid                                        := bpd_update_mispredict && (ras_update_info.attribute.isCall || ras_update_info.attribute.isReturn)
  io.toBPU.ras_updateInfo.bits.rasState                                := RegNext(redirect_mem(ftqMemIdx(bpd_update_ptr)).rasState)
  io.toBPU.ras_updateInfo.bits.attribute                               := (ras_update_info.attribute)
  io.toBPU.ras_updateInfo.bits.cfiPosition                             := ras_update_info.cfiPosition.bits
  io.toBPU.ras_updateInfo.bits.startPc                                  := (bpd_update_pc)


/* -------------------------------------------------------------------------- */
/*                                 To Prefetch                                */
/* -------------------------------------------------------------------------- */

  io.toPrefetch.valid := (!isEmpty(pf_ptr,bpu_ptr)) && !(io.fromBackend.redirect_val||io.fromIfu.redirect_from_fetch || (bpu_s2_redirect && !isBefore(io.fromBpu.resp.bits.f2.ftq_idx,pf_ptr)))
  io.toPrefetch.bits.fetch_pc := pc_mem(ftqMemIdx(pf_ptr)).start_pc
  io.toPrefetch.bits.mask     := pc_mem(ftqMemIdx(pf_ptr)).mask
/* -------------------------------------------------------------------------- */
/*                                   To IFU                                   */
/* -------------------------------------------------------------------------- */
  io.toIfu.req.valid                      := (!isEmpty(ifu_ptr,bpu_ptr)) && !(io.fromBackend.redirect_val||io.fromIfu.redirect_from_fetch)
  io.toIfu.req.bits.ftq_entry             := pc_mem(ftqMemIdx(ifu_ptr))
  io.toIfu.req.bits.ftq_idx               := ifu_ptr
  io.toIfu.flushByBpu.s2_redirect.valid   := io.fromBpu.resp.bits.f2.hasRedirect
  io.toIfu.flushByBpu.s2_redirect.bits    := io.fromBpu.resp.bits.f2.ftq_idx
  io.toIfu.flushByBpu.s3_redirect.valid   := false.B
  io.toIfu.flushByBpu.s3_redirect.bits    := DontCare
  io.toIfu.redirect.valid                 := io.fromBackend.redirect_val
  io.toIfu.redirect.bits                  := io.fromBackend.redirect_ftq_idx



  io.toIfu.req.bits.ftq_offset.valid      := cfi_offset(ftqMemIdx(ifu_ptr)).valid
  io.toIfu.req.bits.ftq_offset.bits       := cfi_offset(ftqMemIdx(ifu_ptr)).bits
  /* -------------------------------------------------------------------------- */
  /*                                 TO Backend                                 */
  /* -------------------------------------------------------------------------- */
  for (i <- 0 until 2) {
    val idx = ftqMemIdx(io.toBackend.get_ftq_pc(i).ftq_idx)
    val next_ptr = CirWrapInc(io.toBackend.get_ftq_pc(i).ftq_idx)
    val next_idx = ftqMemIdx(next_ptr)
    val next_is_enq = (next_ptr === bpu_ptr) && io.fromBpu.resp.fire
    val next_pc = Mux(next_is_enq, bpu_in_entry.start_pc, pc_mem(next_idx).start_pc)

    io.toBackend.get_ftq_pc(i).pc        := RegNext(pc_mem(idx).start_pc)
    io.toBackend.get_ftq_pc(i).next_pc   := RegNext(next_pc)
    io.toBackend.get_ftq_pc(i).next_val  := RegNext((!isFull(next_ptr,bpu_ptr)) || next_is_enq)
    io.toBackend.get_ftq_pc(i).com_pc    := RegNext(pc_mem(ftqMemIdx(Mux(io.fromBackend.deq.valid, io.fromBackend.deq.bits, commit_ptr))).start_pc)
    io.toBackend.get_ftq_pc(i).entry     := RegNext(pd_mem(idx))
    io.toBackend.get_ftq_pc(i).ghist     := RegNext(redirect_mem(ftqMemIdx(bpd_update_ptr)).ghist)//TODO:fix it
  }

  io.debug_fetch_pc := DontCare

}
