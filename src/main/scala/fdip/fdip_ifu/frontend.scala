//******************************************************************************
// Copyright (c) 2017 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Frontend
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.fdip.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._

import boom.fdip.common._
import boom.fdip.exu.{CommitExceptionSignals, BranchDecode, BrUpdateInfo, BranchDecodeSignals}
import boom.fdip.util._
import boom.fdip.ifu.bpd.BranchPredictor
// import boom.fdip.ifu._
class BranchAttribute extends Bundle {
  val branchType: UInt = UInt(2.W)
  val rasAction:  UInt = UInt(2.W)

  def isNone:        Bool = branchType === BranchAttribute.BranchType.None
  def isConditional: Bool = branchType === BranchAttribute.BranchType.Conditional
  def isDirect:      Bool = branchType === BranchAttribute.BranchType.Direct
  def isIndirect:    Bool = branchType === BranchAttribute.BranchType.Indirect

  def isOtherIndirect: Bool = isIndirect && rasAction === BranchAttribute.RasAction.None

  // NOTE: maybe we should check branchType === BranchAttribute.BranchType.Direct/Indirect,
  //       but as BranchAttribute.BranchType is declared as private,
  //       we should not able to create attribute with (Conditional, Push) or something like that.
  //       So, just check rasAction should be enough.
  def isCall:          Bool = rasAction === BranchAttribute.RasAction.Push
  def isReturn:        Bool = rasAction === BranchAttribute.RasAction.Pop
  def isReturnAndCall: Bool = rasAction === BranchAttribute.RasAction.PopAndPush

  // hasPop = isPop || isPushAndPop, hasPush = isPush || isPushAndPop
  def hasPop:  Bool = rasAction(BranchAttribute.RasAction.popBit)
  def hasPush: Bool = rasAction(BranchAttribute.RasAction.pushBit)

  def needIttage: Bool = isIndirect && !hasPop

  def =/=(that: BranchAttribute): Bool =
    this.branchType =/= that.branchType || this.rasAction =/= that.rasAction
}

object BranchAttribute {
  object BranchType {
    private val width = 2

    def apply(): UInt = 0.U(width.W)
    // no branch
    def None: UInt = 0.U(width.W)
    // conditional branches: beq, bne, blt, bge, bltu, bgeu
    def Conditional: UInt = 1.U(width.W)
    // direct branches: j, jal
    def Direct: UInt = 2.U(width.W)
    // indirect branches: jr, jalr
    def Indirect: UInt = 3.U(width.W)

    def assertLegal(value: UInt): Unit = {
      assert(value < 4.U)
    }
  }
  object RasAction {
    private val width = 2
    def popBit:  Int = 0
    def pushBit: Int = 1

    def apply(): UInt = 0.U(width.W)
    // no action
    def None: UInt = 0.U(width.W)
    // special indirect branches: return/call, refer to risc-v spec Table3. Return-address stack prediction hints
    // return: jalr with rs1=x1/x5 and rd!=x1/x5
    def Pop: UInt = (1 << popBit).U(width.W)
    // call: jalr with rd=x1/x5 and rs1!=x1/x5; or rd=x1/x5 and rs1=rd. Or jal with rd=x1/x5
    def Push: UInt = (1 << pushBit).U(width.W)
    // return & call: jalr with rd=x1/x5 and rs1=x1/x5 and rs1!=rd
    def PopAndPush: UInt = ((1 << popBit) | (1 << pushBit)).U(width.W)

    def apply(pop: Bool, push: Bool): UInt = push ## pop

    def assertLegal(value: UInt): Unit = {
      assert(value < 4.U)
    }
  }
  def apply(branchType: UInt, rasAction: UInt, canAssert: Bool = true.B): BranchAttribute = {
    // for some register, we allow X-state on init, so we assert only when canAssert (e.g. stage valid)
    when(canAssert) {
      BranchType.assertLegal(branchType)
      RasAction.assertLegal(rasAction)
    }
    val e = Wire(new BranchAttribute)
    e.branchType := branchType
    e.rasAction  := rasAction
    e
  }

  def decode(inst: UInt, canAssert: Bool = true.B): BranchAttribute = {
    def isRVC(inst: UInt): Bool = inst(1, 0) =/= 3.U

    def isLink(reg: UInt): Bool = reg === 1.U || reg === 5.U

    val branchType = ListLookup(inst, List(BranchType.None), BranchAttribute.brTable).head

    // for jal/jalr, rd=inst(11, 7)
    // for c.jal/jalr, rd is always x1
    // for c.j/jr, rd is always x0
    // we can use inst(12) (funct4(0)) to distinguish c.jal/c.jalr (=1) and c.j/c.jr (=0)
    val rd = Mux(isRVC(inst), inst(12), inst(11, 7))
    // for jal/jalr, rs=inst(19, 15)
    // for c.jal/jalr, rs=inst(11, 7)
    // for c.j/jr, we don't have rs
    val rs = Mux(isRVC(inst), Mux(branchType === BranchType.Direct, 0.U, inst(11, 7)), inst(19, 15))

    // refer to risc-v spec Table3. Return-address stack prediction hints
    val hasPush =
      // we do not support RV32C here, and in RV64C, c.jal should be decoded as c.addiw, so we ask !RVC here
      branchType === BranchType.Direct && isLink(rd) && !isRVC(inst) ||
        branchType === BranchType.Indirect && isLink(rd)
    val hasPop =
      branchType === BranchType.Indirect && isLink(rs) && rd =/= rs

    val rasAction = Cat(hasPush, hasPop)

    apply(branchType, rasAction, canAssert)
  }

  def None:          BranchAttribute = apply(BranchType.None, RasAction.None)
  def Conditional:   BranchAttribute = apply(BranchType.Conditional, RasAction.None)
  def DirectCall:    BranchAttribute = apply(BranchType.Direct, RasAction.Push)
  def IndirectCall:  BranchAttribute = apply(BranchType.Indirect, RasAction.Push)
  def Return:        BranchAttribute = apply(BranchType.Indirect, RasAction.Pop)
  def ReturnAndCall: BranchAttribute = apply(BranchType.Indirect, RasAction.PopAndPush)
  def OtherDirect:   BranchAttribute = apply(BranchType.Direct, RasAction.None)
  def OtherIndirect: BranchAttribute = apply(BranchType.Indirect, RasAction.None)

  private val brTable = Array(
    // c.ebreak must take priority over c.jalr/c.jr.
    BitPat("b????????????????_100_?_00_000_00_000_10") -> List(BranchType.None),
    BitPat("b????????????????_101_?_??_???_??_???_01") -> List(BranchType.Direct),
    BitPat("b????????????????_100_?_??_???_00_000_10") -> List(BranchType.Indirect),
    BitPat("b????????????????_11?_?_??_???_??_???_01") -> List(BranchType.Conditional),
    BitPat("b????????????????_???_?????_1101111")       -> List(BranchType.Direct),
    BitPat("b????????????????_000_?????_1100111")       -> List(BranchType.Indirect),
    BitPat("b????????????????_???_?????_1100011")       -> List(BranchType.Conditional)
  )
}
class FrontendResp(implicit p: Parameters) extends BoomBundle()(p) {
  val pc = UInt(vaddrBitsExtended.W)  // ID stage PC
  val data = UInt((fetchWidth * coreInstBits).W)
  val mask = UInt(fetchWidth.W)
  val xcpt = new FrontendExceptions
  val ghist = new GlobalHistory

  // fsrc provides the prediction FROM a branch in this packet
  // tsrc provides the prediction TO this packet
  val fsrc = UInt(BSRC_SZ.W)
  val tsrc = UInt(BSRC_SZ.W)
}


/**
 * Parameters to manage a L1 Banked ICache
 */
trait HasBoomFrontendParameters extends HasL1ICacheParameters
{
  // Number of low address bits occupied by the minimum instruction alignment.
  // Keep this as a shared parameter so BTB target slicing uses the same
  // alignment as the rest of the frontend.
  val instOffsetBits = log2Ceil(coreInstBytes)


  // How many banks does the ICache use?
  val nBanks = fetchWidth 
  /*
  //IBTB 默认要求fetchwidth和bank数目相等，且fetchwidth一定小于等于predWidth，不然会出现bank跨block的问题，导致fetch预测不完全
  RBTB：默认要求fetchwidth*2<= blockbytes//一个block 2个分支槽
  BBTB:默认的索引可以自定义配置（看是否设置bank），理论上每一个start_block,都含有一个表项（存储内容和RBTB类似，存储方式和IBTB类似）
  
  */
  // How many bytes wide is a bank?
  // println(s"nBanks: $nBanks")
  val bankBytes = fetchBytes/nBanks

  val predWidth = fetchWidth
  val numBr     = 2
  // require(nBanks == 1 || nBanks == 2)


  def BP_STAGES = (0 until 3).map(_.U(2.W))
  def BP_S1     = BP_STAGES(0)
  def BP_S2     = BP_STAGES(1)
  def BP_S3     = BP_STAGES(2)

  def Fault     = (0 until 5).map(_.U(3.W))
  def jal_fault = Fault(0)
  def ret_fault = Fault(1)
  def target_fault = Fault(2)
  def notcfi_fault = Fault(3)
  def notvalid_fault = Fault(4)
  // How many "chunks"/interleavings make up a cache line?
  val numChunks = cacheParams.blockBytes / bankBytes

  // Which bank is the address pointing to?


  def blockAlign(addr: UInt)  = ~(~addr | (cacheParams.blockBytes-1).U)
  def bankAlign(addr: UInt)   = ~(~addr | (bankBytes-1).U)
  def fetchAlign(addr: UInt)  = ~(~addr | (fetchBytes-1).U)
  def fetchIdx(addr: UInt)    = addr >> log2Ceil(fetchBytes)

  // def nextBank(addr: UInt) = bankAlign(addr) + bankBytes.U
  def nextFetch(addr: UInt) = {
    fetchAlign(addr) + fetchBytes.U
  }

  def fetchMask(addr: UInt) = {
    val idx = addr.extract(log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))

    ((1 << fetchWidth)-1).U << idx
    
  }

  // def bankMask(addr: UInt) = {
  //   val idx = addr.extract(log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))
  //   if (nBanks == 1) {
  //     1.U(1.W)
  //   } else {
  //     Mux(mayNotBeDualBanked(addr), 1.U(2.W), 3.U(2.W))
  //   }
  // }
}



/**
 * Bundle passed into the FetchBuffer and used to combine multiple
 * relevant signals together.
 */
class FetchBundle(implicit p: Parameters) extends BoomBundle
  with HasBoomFrontendParameters
{
  //Bank will delete ,we need 8 bank for fetch not 2 bank ,each bank 4 inst
  val pc            = UInt(vaddrBitsExtended.W)
  val next_pc       = UInt(vaddrBitsExtended.W)
  val edge_inst     = Bool()// True if 1st instruction in this bundle is pc - 2
  val insts         = Vec(fetchWidth, Bits(32.W))
  val exp_insts     = Vec(fetchWidth, Bits(32.W))

  // Information for sfb folding
  // NOTE: This IS NOT equivalent to uop.pc_lob, that gets calculated in the FB
  val sfbs                 = Vec(fetchWidth, Bool())
  val sfb_masks            = Vec(fetchWidth, UInt((2*fetchWidth).W))
  val sfb_dests            = Vec(fetchWidth, UInt((1+log2Ceil(fetchBytes)).W))
  val shadowable_mask      = Vec(fetchWidth, Bool())
  val shadowed_mask        = Vec(fetchWidth, Bool())

  val cfiPosition       = Valid(UInt(log2Ceil(fetchWidth).W))
  val attribute      = new BranchAttribute


  // val ras_top       = UInt(vaddrBitsExtended.W)

  val ftq_idx       = UInt((log2Ceil(ftqSz)+1).W)
  val mask          = UInt(fetchWidth.W) // mark which words are valid instructions

  val br_mask       = UInt(fetchWidth.W)

  // val ghist         = new GlobalHistory
  // val lhist         = Vec(nBanks, UInt(localHistoryLength.W))

  val xcpt_pf_if    = Bool() // I-TLB miss (instruction fetch fault).
  val xcpt_ae_if    = Bool() // Access exception.

  val bp_debug_if_oh= Vec(fetchWidth, Bool())
  val bp_xcpt_if_oh = Vec(fetchWidth, Bool())

  val end_half      = Valid(UInt(16.W))
}


/**
 * IO for the BOOM Frontend to/from the CPU
 */
class BoomFrontendIO(implicit p: Parameters) extends BoomBundle
{
  // Give the backend a packet of instructions.
  val fetchpacket       = Flipped(new DecoupledIO(new FetchBufferResp))

  // 1 for xcpt/jalr/auipc/flush
  val get_pc            = Flipped(Vec(2, new GetPCFromFtqIO()))
  val debug_ftq_idx     = Output(Vec(coreWidth, UInt((log2Ceil(ftqSz)+1).W)))
  val debug_fetch_pc    = Input(Vec(coreWidth, UInt(vaddrBitsExtended.W)))

  // Breakpoint info
  val status            = Output(new MStatus)
  val bp                = Output(Vec(nBreakpoints, new BP))
  val mcontext          = Output(UInt(coreParams.mcontextWidth.W))
  val scontext          = Output(UInt(coreParams.scontextWidth.W))

  val sfence            = Valid(new SFenceReq)//TODO: need to handle it 

  val brupdate          = Output(new BrUpdateInfo)

  // Redirects change the PC
  val redirect_flush   = Output(Bool()) // Flush and hang the frontend?
  val redirect_val     = Output(Bool()) // Redirect the frontend?
  val redirect_pc      = Output(UInt()) // Where do we redirect to?
  val redirect_ftq_idx = Output(UInt()) // Which ftq entry should we reset to?
  val redirect_ghist   = Output(new GlobalHistory) // What are we setting as the global history?

  val commit = Valid(UInt((log2Ceil(ftqSz)+1).W))

  val flush_icache = Output(Bool())

  val perf = new Bundle{
    val ftq_redirect_conflict = Input(Bool())
    val ftq_meta_conflict     = Input(Bool())
    val ftq_full              = Input(Bool())
    val icache_miss           = Input(Bool())
    val icache_prefetch       = Input(Bool())
  }
}

/**
 * Top level Frontend class
 *
 * @param icacheParams parameters for the icache
 * @param hartid id for the hardware thread of the core
 */
class FDIPBoomFrontend(val icacheParams: ICacheParams, staticIdForMetadataUseOnly: Int)(implicit p: Parameters) extends LazyModule
{
  lazy val module = new BoomFrontendModule(this)
  val icache = LazyModule(new boom.fdip.ifu.ICache(icacheParams, staticIdForMetadataUseOnly))
  val masterNode = icache.masterNode
  val resetVectorSinkNode = BundleBridgeSink[UInt](Some(() =>
    UInt(masterNode.edges.out.head.bundle.addressBits.W)))
}

/**
 * Bundle wrapping the IO for the Frontend as a whole
 *
 * @param outer top level Frontend class
 */
class BoomFrontendBundle(val outer: FDIPBoomFrontend) extends CoreBundle()(outer.p)
{
  val cpu = Flipped(new BoomFrontendIO())
  val ptw = new TLBPTWIO()
  //perf


}

/**
 * Main Frontend module that connects the icache, TLB, fetch controller,
 * and branch prediction pipeline together.
 *
 * @param outer top level Frontend class
 */
class BoomFrontendModule(outer: FDIPBoomFrontend) extends LazyModuleImp(outer)
  with HasBoomCoreParameters
  with HasBoomFrontendParameters
{
  val io = IO(new BoomFrontendBundle(outer))
  val io_reset_vector = outer.resetVectorSinkNode.bundle
  implicit val edge = outer.masterNode.edges.out(0)
  require(fetchWidth*coreInstBytes == outer.icacheParams.fetchBytes)



  val icache = outer.icache.module
  icache.io.invalidate := io.cpu.flush_icache
  val tlb = Module(new FDIPTLB(true, log2Ceil(fetchBytes), TLBConfig(nTLBSets, nTLBWays)))
  io.ptw <> tlb.io.ptw
  // io.cpu.perf.tlbMiss := io.ptw.req.fire
  // io.cpu.perf.acquire := false.B


  val bpu = Module(new BranchPredictor)
  val ifu = Module(new BoomIFU)
  val fb  = Module(new FetchBuffer)
  val ftq = Module(new FDIPFetchTargetQueue)
  val flush_frontend = io.cpu.redirect_val 
  val flush_bef_s2 = ifu.io.toFtq.redirect_from_fetch
  icache.io.req.valid           := ftq.io.toICache.req.valid && !(flush_bef_s2||flush_frontend)
  icache.io.req.bits.addr       := ftq.io.toICache.req.bits.fetch_pc

  icache.io.prefetch_req.valid := ftq.io.toPrefetch.valid && !(flush_bef_s2||flush_frontend)
  icache.io.prefetch_req.bits := ftq.io.toPrefetch.bits


  ftq.io.toICache.req.ready     := icache.io.req.ready
  ifu.io.fromICache.ready       := icache.io.req.ready
  ftq.io.toPrefetch.ready    := icache.io.prefetch_req.ready
  ifu.io.fromICache.data_valid  := icache.io.resp.valid
  ifu.io.fromICache.data        := icache.io.resp.bits.data
  ifu.io.fromICache.ae          := icache.io.resp.bits.ae
  ifu.io.fromICache.replay      := icache.io.resp.bits.replay//TODO：delete it
  icache.io.ifu_s3_ready        := ifu.io.toICache.f3_ready

  tlb.io.req(0).valid             := icache.io.prefetch_tlb_req.valid
  tlb.io.req(0).bits.cmd          := DontCare
  tlb.io.req(0).bits.vaddr        := icache.io.prefetch_tlb_req.bits.fetch_pc
  tlb.io.req(0).bits.passthrough  := false.B
  tlb.io.req(0).bits.size         := log2Ceil(coreInstBytes * fetchWidth).U
  tlb.io.req(0).bits.v            := io.ptw.status.v
  tlb.io.req(0).bits.prv          := io.ptw.status.prv

  tlb.io.req(1) := DontCare
    
  tlb.io.sfence                := RegNext(io.cpu.sfence)
  tlb.io.kill                  := false.B
  icache.io.prefetch_tlb_resp := tlb.io.resp(0)
  icache.io.flush    := io.cpu.redirect_val || ifu.io.toFtq.redirect_from_fetch

  ftq.io.toIfu       <> ifu.io.fromFtq
  ftq.io.toBPU       <> bpu.io.fromFtq
  ftq.io.fromBpu     <> bpu.io.toFtq
  ftq.io.fromIfu     <> ifu.io.toFtq
  ftq.io.fromBackend.redirect_val     := io.cpu.redirect_val
  ftq.io.fromBackend.redirect_pc      := io.cpu.redirect_pc
  ftq.io.fromBackend.redirect_ftq_idx := io.cpu.redirect_ftq_idx
  ftq.io.fromBackend.deq              := io.cpu.commit
  ftq.io.fromBackend.brupdate      <> io.cpu.brupdate
  io.cpu.get_pc           <> ftq.io.toBackend.get_ftq_pc


  bpu.io.reset_vector := io_reset_vector
  bpu.io.f3_fire      := DontCare
  ifu.io.fromTLB.xcpt_pf_if := tlb_resp.ae.inst 
  ifu.io.fromTLB.xcpt_ae_if := tlb_resp.pf.inst

  fb.io.enq <> ifu.io.toIbuf.req
  fb.io.clear := io.cpu.redirect_flush || io.cpu.redirect_val//TODO check it
  io.cpu.fetchpacket <> fb.io.deq

  io.cpu.perf.ftq_full              := ftq.io.ftq_full
  io.cpu.perf.ftq_meta_conflict     := ftq.io.ftq_meta_conflict
  io.cpu.perf.ftq_redirect_conflict := ftq.io.ftq_redirect_conflict
  io.cpu.perf.icache_miss           := icache.io.perf.miss_acquire
  io.cpu.perf.icache_prefetch       := icache.io.perf.prefetch_acquire

  io.cpu.debug_fetch_pc             := ftq.io.debug_fetch_pc
  ftq.io.debug_ftq_idx              := io.cpu.debug_ftq_idx
  // io.cpu.debug_ftq_idx  := ftq.io.toBackend.debug_ftq_idx
}
