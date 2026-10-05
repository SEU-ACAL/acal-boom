package boom.fdip.ifu.bpd.ras

import chisel3._
import chisel3.util._
import boom.fdip.common._
import boom.fdip.ifu.BranchAttribute
import boom.fdip.ifu.bpd.common.CircularQueuePtr
import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._


class RasPtr(entries: Int)(implicit p: Parameters)
  extends CircularQueuePtr[RasPtr](entries) {
  override protected def make(f: Bool, v: UInt): RasPtr = {
    val ptr = Wire(new RasPtr(entries))
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

object RasPtr {
  def apply(flag: Bool, value: UInt, entries: Int)(implicit p: Parameters): RasPtr = {
    val ptr = Wire(new RasPtr(entries))
    ptr.flag := flag
    ptr.value := value
    ptr
  }
}

class RasEntry(implicit p: Parameters) extends RasBundle {
  val retAddr = UInt(vaddrBitsExtended.W) // return address
  val ctr:     UInt      = UInt(RasStackCounterWidth.W) // layer of nested call functions
}

object RasEntry {
  def apply(retAddr: UInt, ctr: UInt)(implicit p: Parameters): RasEntry = {
    val e = Wire(new RasEntry)
    e.retAddr := retAddr
    e.ctr     := ctr
    e
  }
}

class RasInternalMeta(implicit p: Parameters) extends RasBundle {
  val ssp:  UInt   = UInt(log2Up(RasCommitStackSize).W)
  val sctr: UInt   = UInt(RasStackCounterWidth.W)
  val tosw: RasPtr = new RasPtr(RasSpecQueueSize)
  val tosr: RasPtr = new RasPtr(RasSpecQueueSize)
  val nos:  RasPtr = new RasPtr(RasSpecQueueSize)
}

object RasInternalMeta {
  def apply(ssp: UInt, sctr: UInt, tosw: RasPtr, tosr: RasPtr, nos: RasPtr)(implicit p: Parameters): RasInternalMeta = {
    val e = Wire(new RasInternalMeta)
    e.ssp  := ssp
    e.sctr := sctr
    e.tosw := tosw
    e.tosr := tosr
    e.nos  := nos
    e
  }
}
/** State before this fetch block's RAS operation, including its return target. */
class RasMeta(implicit p: Parameters) extends RasInternalMeta {
  val topRetAddr  = UInt(vaddrBitsExtended.W)
}
class RasRedirectMeta(implicit p: Parameters) extends RasMeta
//used for update commit stack
class RasCommitMeta(implicit p: Parameters) extends RasBundle {
  val ssp:  UInt   = UInt(log2Up(RasCommitStackSize).W)
  val tosw: RasPtr = new RasPtr(RasSpecQueueSize)
}

object RasCommitMeta {
  def apply(ssp: UInt, tosw: RasPtr)(implicit p: Parameters): RasCommitMeta = {
    val e = Wire(new RasCommitMeta)
    e.ssp  := ssp
    e.tosw := tosw
    e
  }
}

class RasBypass(implicit p: Parameters) extends RasBundle {
  val taken:       Bool            = Bool()
  val cfiPosition: UInt            = UInt(log2Ceil(fetchBytes).W)
  val attribute:   BranchAttribute = new BranchAttribute
}

class RasSpecInfo(implicit p: Parameters) extends RasBundle {
  val attribute:   BranchAttribute = new BranchAttribute
  val cfiPosition: UInt            = UInt(log2Ceil(fetchBytes).W)
  val startPc:     UInt            = UInt(vaddrBitsExtended.W)
}

/** Ordered FTQ retirement, independent of branch-predictor training valid. */
class RasCommitInfo(implicit p: Parameters) extends RasBundle {
  val attribute = new BranchAttribute
  val cfiPosition = UInt(log2Ceil(fetchBytes).W)
  val startPc = UInt(vaddrBitsExtended.W)
  val cfiValid = Bool()
  val taken = Bool()
  val meta = new RasCommitMeta
}


class RasSpecIO(implicit p: Parameters) extends RasBundle {
    val fire:      Bool      = Input(Bool())
    val pushValid: Bool      = Input(Bool())
    val popValid:  Bool      = Input(Bool())
    val pushAddr = Input(UInt(vaddrBitsExtended.W))
    val popAddr = Output(UInt(vaddrBitsExtended.W))
}

class RasCommitIO(implicit p: Parameters) extends RasBundle {
    val valid:     Bool      = Input(Bool())
    val pushValid: Bool      = Input(Bool())
    val popValid:  Bool      = Input(Bool())
    val pushAddr = Input(UInt(vaddrBitsExtended.W))
    val metaTosw:  RasPtr    = Input(new RasPtr(RasSpecQueueSize))
    // for debug purpose only
    val metaSsp: UInt = Input(UInt(log2Up(RasCommitStackSize).W))
}

class RasRedirectIO(implicit p: Parameters) extends RasBundle {
    val valid:    Bool            = Input(Bool())
    val isCall:   Bool            = Input(Bool())
    val callAddr = Input(UInt(vaddrBitsExtended.W))
    val isRet:    Bool            = Input(Bool())
    val meta:     RasInternalMeta = Input(new RasInternalMeta)
}
