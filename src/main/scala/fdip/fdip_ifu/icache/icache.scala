//******************************************************************************
// Copyright (c) 2017 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// ICache
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.fdip.ifu

import chisel3._
import chisel3.util._
import chisel3.util.random._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import freechips.rocketchip.rocket.{HasL1ICacheParameters, ICacheParams, ICacheErrors, ICacheReq, PRV, TLBReq, TLBResp}
import boom.fdip.ifu._



import boom.fdip.common._
import boom.fdip.util._
import freechips.rocketchip.tilelink.TLMessages.d

/**
 * ICache module
 *
 * @param icacheParams parameters for the icache
 * @param hartId the id of the hardware thread in the cache
 * @param enableBlackBox use a blackbox icache
 */
class ICache(
  val icacheParams: ICacheParams,
  val staticIdForMetadataUseOnly: Int)(implicit p: Parameters)
  extends LazyModule
{
  lazy val module = new ICacheLazyModule(this)
  val masterNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    sourceId = IdRange(0, 12), // 12 个mshr 4fetch 8 prefetch//prefetch代表预取深度
    name = s"Core ${staticIdForMetadataUseOnly} ICache")))))
  val size = icacheParams.nSets * icacheParams.nWays * icacheParams.blockBytes
  private val wordBytes = icacheParams.fetchBytes
}

/**
 * IO Signals leaving the ICache
 *
 * @param outer top level ICache class
 */
class ICacheResp(val outer: ICache) extends Bundle
{
  val data    = UInt((outer.icacheParams.fetchBytes*8).W)
  val replay  = Bool()
  val ae      = Bool()
}

/**
 * IO Signals for interacting with the ICache
 *
 * @param outer top level ICache class
 */
class ICacheIOBundle(val outer: ICache) extends BoomBundle()(outer.p)
  with HasBoomFrontendParameters
{
  val req               = Flipped(Decoupled(new ICacheReq))
  val prefetch_req      = Flipped(DecoupledIO(new FTQToPrefetchIO))
  val prefetch_tlb_req  = DecoupledIO(new TLBReq(log2Ceil(fetchBytes)))
  val prefetch_tlb_resp = Input(new TLBResp(log2Ceil(fetchBytes)))
  val prefetch_tlb_v    = Input(Bool())
  val prefetch_tlb_prv  = Input(UInt(PRV.SZ.W))
  // val s1_prefetch_kill  = Input(Bool()) // 此时发生tlb miss，预取失效
  // val s1_kill           = Input(Bool()) // delayed one cycle w.r.t. req
  // val s2_kill           = Input(Bool()) // delayed two cycles; prevents I$ miss emission

  val resp              = Valid(new ICacheResp(outer))
  val ifu_s3_ready      = Input(Bool())
  val flush             = Input(Bool())//后端重定向以及前端的重定向
  val invalidate        = Input(Bool())

  val perf = Output(new Bundle {
    val miss_acquire = Bool()
    val prefetch_acquire = Bool()
  })
}

/**
 * Get a tile-specific property without breaking deduplication
 */
object GetPropertyByHartId
{
  def apply[T <: Data](tiles: Seq[RocketTileParams], f: RocketTileParams => Option[T], hartId: UInt): T = {
    PriorityMux(tiles.collect { case t if f(t).isDefined => (t.tileId.U === hartId) -> f(t).get })
  }
}


 
/**
 * Main ICache module
 *
 * @param outer top level ICache class
 */

//现在row bits已经没什么用
class ICacheLazyModule(outer: ICache) extends LazyModuleImp(outer)
  with HasICacheParameters
{

  val io = IO(new ICacheIOBundle(outer))
  val (tl_out, edge_out) = outer.masterNode.out(0)
  override val refillCycles = outer.icacheParams.blockBytes * 8 / tl_out.d.bits.data.getWidth

  val missUnit = Module(new ICacheMissUnit(edge_out, refillCycles))
  val mainPipe = Module(new MainPipe())
  val prefetchPipe = Module(new PrefetchPipe())
  val wayLookup = Module(new WayLookup())

  // The metadata array is single-ported.  Refill writes have highest
  // priority; normal and prefetch lookups share the remaining read port.
  val tagArray = SyncReadMem(nSets, Vec(nWays, UInt(tagBits.W)))
  val dataArrays = (0 until nWays).map { way =>
    (0 until numBanks).map { bank =>
      Module(new ICacheSRAMHelper(
        "ICache Data Array", way, bank, nSets,
        UInt((cacheBlockBytes * 8 / numBanks).W)))
    }
  }
  val validBits = RegInit(0.U((nSets * nWays).W))

  val missResp = missUnit.io.fetch_resp
  val refillStartMask = missUnit.io.refill_start.map { start =>
    Mux(start.valid, UIntToOH(Cat(start.bits.way, start.bits.vSetIdx), nSets * nWays), 0.U((nSets * nWays).W))
  }.reduce(_ | _)
  val refillDoneMask = Mux(missResp.valid && !io.invalidate,
    UIntToOH(Cat(missResp.bits.way, missResp.bits.vSetIdx), nSets * nWays),
    0.U((nSets * nWays).W))

  when (io.invalidate) {
    validBits := 0.U
  }.elsewhen (refillStartMask.orR || refillDoneMask.orR) {
    validBits := (validBits & ~refillStartMask) | (refillDoneMask & ~refillStartMask)
  }
  // A refill owns the metadata port for the whole refill transaction and for
  // the response/write cycle.  This prevents a read/write collision on the
  // single-ported tag array.
  val metaRefillBusy = missUnit.io.refill_busy || missResp.valid
  // Keep ready asserted even when the prefetch request is not yet valid;
  // PrefetchPipe uses this signal to accept its S0 transaction.  Refill owns
  // the port whenever it is active.
  prefetchPipe.io.metaRead.req.ready := !metaRefillBusy && !io.flush && !io.invalidate
  val prefetchMetaIdx = RegEnable(
    prefetchPipe.io.metaRead.req.bits.vSetIdx,
    prefetchPipe.io.metaRead.req.fire)

  val prefetchMetaReadFire = prefetchPipe.io.metaRead.req.fire
  val metaRead = tagArray.read(
    prefetchPipe.io.metaRead.req.bits.vSetIdx, prefetchMetaReadFire)
  when (missResp.valid && !io.invalidate) {
    tagArray.write(missResp.bits.vSetIdx,
      VecInit(Seq.fill(nWays)(missResp.bits.blkPaddr >> idxBits)),
      Seq.tabulate(nWays)(missResp.bits.way === _.U))
  }

  val prefetchTagReadValid = RegNext(prefetchMetaReadFire, false.B)
  for (way <- 0 until nWays) {
    prefetchPipe.io.metaRead.resp(way).valid := prefetchTagReadValid &&
      validBits(Cat(way.U, prefetchMetaIdx))
    prefetchPipe.io.metaRead.resp(way).bits.pTag := metaRead(way)
  }
  // WayLookup entries are produced only by the prefetch pipeline.  Refill
  // responses update existing entries through the dedicated update port.
  prefetchPipe.io.wayLookupWrite <> wayLookup.io.write

  for (way <- 0 until nWays) {
    for (bank <- 0 until numBanks) {
      val array = dataArrays(way)(bank)
      array.io.w.wen := missResp.valid && !io.invalidate && missResp.bits.way === way.U
      array.io.w.waddr := missResp.bits.vSetIdx
      array.io.w.wdata := missResp.bits.data((bank + 1) * bankBits - 1, bank * bankBits)
      array.io.r.req.valid := mainPipe.io.mainPipeDataReq.valid &&
        mainPipe.io.mainPipeDataReq.bits.way === way.U
      array.io.r.req.bits.raddr := mainPipe.io.mainPipeDataReq.bits.vSetIdx
    }
  }
  val dataReadFire = mainPipe.io.mainPipeDataReq.fire
  //read en
  mainPipe.io.mainPipeDataReq.ready := dataArrays.map(_.map(_.io.r.req.ready).reduce(_ && _)).reduce(_ && _)
  val mainData = Wire(Vec(numBanks, UInt(bankBits.W)))
  for (bank <- 0 until numBanks) {
    mainData(bank) := Mux1H((0 until nWays).map(way =>
      (RegNext(mainPipe.io.mainPipeDataReq.bits.way) === way.U) -> dataArrays(way)(bank).io.r.resp.rdata))
  }
  mainPipe.io.mainPipeDataResp.valid := RegNext(dataReadFire, false.B)
  mainPipe.io.mainPipeDataResp.bits.data := Cat(mainData.reverse)

  prefetchPipe.io.fromFtq.valid := io.prefetch_req.valid
  prefetchPipe.io.fromFtq.bits := io.prefetch_req.bits
  io.prefetch_req.ready := prefetchPipe.io.fromFtq.ready
  prefetchPipe.io.flush := io.flush || io.invalidate

  io.prefetch_tlb_req.valid := prefetchPipe.io.tlbReq.valid
  io.prefetch_tlb_req.bits := prefetchPipe.io.tlbReq.bits
  prefetchPipe.io.tlbReq.ready := io.prefetch_tlb_req.ready
  prefetchPipe.io.tlbResp := io.prefetch_tlb_resp
  prefetchPipe.io.ptw.prv := io.prefetch_tlb_prv
  prefetchPipe.io.ptw.v := io.prefetch_tlb_v
  prefetchPipe.io.missResp.valid := missResp.valid
  prefetchPipe.io.missResp.bits := missResp.bits


  wayLookup.io.flush := io.flush || io.invalidate
  wayLookup.io.update.valid := missResp.valid
  wayLookup.io.update.bits := missResp.bits
  wayLookup.io.mainPipeToWayLookup := mainPipe.io.wayLookupToMainPipe.fire
  mainPipe.io.wayLookupToMainPipe <> wayLookup.io.wayLookupToMainPipe

  mainPipe.io.fromFtq.valid := io.req.valid && !io.flush && !metaRefillBusy
  mainPipe.io.fromFtq.bits.fetch_pc := io.req.bits.addr
  mainPipe.io.fromFtq.bits.ftqIdx := 0.U
  io.req.ready := mainPipe.io.fromFtq.ready && !io.flush && !io.invalidate &&
    !metaRefillBusy
  mainPipe.io.flush := io.flush || io.invalidate
  mainPipe.io.toIFU.ready := io.ifu_s3_ready

  missUnit.io.fetch_req <> mainPipe.io.mainPipeMissReq
  mainPipe.io.mainPipeMissResp.valid := missResp.valid
  mainPipe.io.mainPipeMissResp.bits := missResp.bits
  missUnit.io.fencei := io.invalidate
  missUnit.io.flush := io.flush || io.invalidate
  missUnit.io.prefetch_req.valid := prefetchPipe.io.missReq.valid
  missUnit.io.prefetch_req.bits := prefetchPipe.io.missReq.bits
  prefetchPipe.io.missReq.ready := missUnit.io.prefetch_req.ready

  //TODO: fix this startbank

  val mainBlockData = mainPipe.io.toIFU.bits.data.asTypeOf(Vec(numBanks, UInt(bankBits.W)))
  val mainStartBank = mainPipe.io.toIFU.bits.startBank
  val mainFetchData = Wire(Vec(fetchBankNum, UInt(bankBits.W)))
  for (i <- 0 until fetchBankNum) {
    mainFetchData(i) := mainBlockData((mainStartBank + i.U) % numBanks.U)
  }
  io.resp.valid := mainPipe.io.toIFU.valid && !io.flush
  io.resp.bits.data := Cat(mainFetchData.reverse)
  io.resp.bits.replay := false.B
  io.resp.bits.ae := false.B
  tl_out.a <> missUnit.io.acquire
  missUnit.io.grant <> tl_out.d
  tl_out.b.ready := true.B
  tl_out.c.valid := false.B
  tl_out.e.valid := false.B
  io.perf.miss_acquire := tl_out.a.fire && tl_out.a.bits.source < 4.U
  io.perf.prefetch_acquire := tl_out.a.fire && tl_out.a.bits.source >= 4.U

  override def toString: String = BoomCoreStringPrefix(
    "==L1-ICache==",
    "Fetch bytes   : " + cacheParams.fetchBytes,
    "Block bytes   : " + (1 << blockOffBits),
    "Row bytes     : " + rowBytes,
    "Word bits     : " + (outer.icacheParams.fetchBytes * 8),
    "Sets          : " + nSets,
    "Ways          : " + nWays,
    "Refill cycles : " + refillCycles,
    "RAMs          : (" + (outer.icacheParams.fetchBytes * 8 / nBanks) + " x " + nSets*refillCycles + ") using " + nBanks + " banks",
    "" + (if (nBanks == 2) "Dual-banked" else "Single-banked"),
    "I-TLB ways    : " + cacheParams.nTLBWays + "\n")
}
