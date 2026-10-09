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
import freechips.rocketchip.rocket.{HasL1ICacheParameters, ICacheParams, ICacheErrors, ICacheReq, PRV}
import boom.fdip.ifu._
import freechips.rocketchip.rocket.{TLBReq,TLBResp}


import boom.fdip.common._
import boom.fdip.util._
import freechips.rocketchip.tilelink.TLMessages.d

class WayLookupEntry(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx = UInt(nSetsBits.W)
  val waymask =  UInt(nWays.W)
  val pTag:        UInt      = UInt(tagBits.W)
  val fetchFault = Bool()
}
class MetaReadBundle(implicit p: Parameters) extends ICacheBundle {
  val req = DecoupledIO(new MetaArrayReq())
  val resp = Vec(nWays,Flipped(ValidIO(new MetaArrayResp())))
}
class PtwReadBundle(implicit p: Parameters) extends ICacheBundle {
  val v  = Bool()
  val prv = UInt(PRV.SZ.W)
}

class MetaArrayReq(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx = UInt(nSetsBits.W)
}
class MetaArrayResp(implicit p: Parameters) extends ICacheBundle {
  val pTag:        UInt      = UInt(tagBits.W)
}
class WayLookupWrite(implicit p: Parameters) extends ICacheBundle {
  val entry:          WayLookupEntry          = new WayLookupEntry
  val ftqIdx = UInt(log2Ceil(ftqSz).W)
}

class WayLookupToMainPipe(implicit p: Parameters) extends ICacheBundle {
  val entry = new WayLookupEntry
  //TODO: add exception signals
}
class FTQToMainPipeIO(implicit p: Parameters) extends BoomBundle
{
  val fetch_pc  = UInt(vaddrBitsExtended.W)
  val ftqIdx = UInt(log2Ceil(ftqSz).W)
}

class ICacheMissReq(implicit p: Parameters) extends ICacheBundle{
    val blkPaddr: UInt = UInt((vaddrBitsExtended - blockOffBits).W)
    val way:  UInt = UInt(log2Ceil(nWays).W)
    val vSetIdx:  UInt = UInt((log2Up(nSets)).W)
}

class ICacheMissResp(implicit p: Parameters) extends ICacheBundle{
  val blkPaddr: UInt = UInt((vaddrBitsExtended - blockOffBits).W)

  val vSetIdx:  UInt = UInt((log2Up(nSets)).W)
  val way:  UInt = UInt(log2Ceil(nWays).W)
  val data:     UInt = UInt((cacheBlockBytes*8).W)
//   val corrupt:  Bool = Bool()
}

class MSHRMissResp(implicit p: Parameters) extends ICacheBundle{
  val blkPaddr: UInt = UInt((vaddrBitsExtended - blockOffBits).W)
  val vSetIdx:  UInt = UInt((log2Up(nSets)).W)
  val way:  UInt = UInt(log2Ceil(nWays).W)
}

class MainPipeDataReq(implicit p: Parameters) extends ICacheBundle{
  val vSetIdx:  UInt = UInt((log2Up(nSets)).W)
  val way:  UInt = UInt(log2Ceil(nWays).W)
}
class MainPipeDataResp(implicit p: Parameters) extends ICacheBundle{
  val data:     UInt = UInt((cacheBlockBytes*8).W)
}
class MainPipeToIFU(implicit p: Parameters) extends ICacheBundle{
  val data:     UInt = UInt((cacheBlockBytes*8).W)
  val startBank: UInt = UInt(bankOffBits.W)
}
class ICacheMetaWriteBundle(implicit p: Parameters) extends ICacheBundle {
    val virIdx      = UInt((log2Up(nSets)).W)
    val phyTag      = UInt((vaddrBitsExtended - blockOffBits).W)
    val way        = UInt(log2Ceil(nWays).W)
}
class ICacheDataWriteBundle(implicit p: Parameters) extends ICacheBundle{
    val virIdx      = UInt(idxBits.W)
    // // val phyTag      = UInt((vaddrBitsExtended - blockOffBits - idxBits).W)
    val way     = UInt(log2Ceil(nWays).W)
    val data        = UInt((cacheBlockBytes*8).W)
}

class ICacheMissUnitIO(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheBundle {

  // There are four demand and eight prefetch MSHRs.  Keeping one start
  // event per MSHR preserves simultaneous demand/prefetch allocations.
  private val nRefillSources = 12

  // control
  val fencei: Bool = Input(Bool())
  val flush:  Bool = Input(Bool())
//   val invalidate: Bool = Input(Bool())

  // fetch
  val fetch_req:  DecoupledIO[ICacheMissReq] = Flipped(DecoupledIO(new ICacheMissReq()))
  val fetch_resp: Valid[ICacheMissResp]      = ValidIO(new ICacheMissResp())
  val refill_busy: Bool = Output(Bool())
  val refill_start = Vec(nRefillSources, ValidIO(new ICacheMissReq()))
  // prefetch
  val prefetch_req: DecoupledIO[ICacheMissReq] = Flipped(DecoupledIO(new ICacheMissReq()))
//   // SRAM Write Req
//   val meta_write: DecoupledIO[ICacheMetaWriteBundle] = DecoupledIO(new ICacheMetaWriteBundle())
//   val data_write: DecoupledIO[ICacheDataWriteBundle] = DecoupledIO(new ICacheDataWriteBundle())
  // get victim from replacer
//   val victim: ReplacerVictim = new ReplacerVictim
  // Tilelink
  val acquire                               = DecoupledIO(new TLBundleA(edge.bundle))
  val grant                                 = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
//   val mem_acquire: DecoupledIO[TLBundleA] = DecoupledIO(new TLBundleA(edge.bundle))
//   val mem_grant:   DecoupledIO[TLBundleD] = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
}
class LookUpMSHR(implicit p: Parameters) extends ICacheBundle{
  val info: Valid[ICacheMissReq] = ValidIO(new ICacheMissReq())
  val hit:  Bool                 = Input(Bool())
}
class ICacheMSHRIO(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheBundle{

  // control
  val fencei: Bool      = Input(Bool())
  val flush:  Bool      = Input(Bool())
  val invalidate: Bool  = Input(Bool())

  // fetch
  val req:  DecoupledIO[ICacheMissReq] = Flipped(DecoupledIO(new ICacheMissReq()))//目前兼容lookup的功能
  val resp: Valid[MSHRMissResp]      = ValidIO(new MSHRMissResp())//返回写入data和tag的必要数据
  val busy: Bool = Output(Bool())

  val lookUps:   Vec[LookUpMSHR]            = Flipped(Vec(2, new LookUpMSHR())) //1:fetch 2:pre-fetch
  val acquire                               = DecoupledIO(new TLBundleA(edge.bundle))
}
