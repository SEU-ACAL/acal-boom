package boom.fdip.ifu.bpd.ras

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import boom.fdip.ifu.bpd.common.HasCircularQueuePtrHelper

/* Persistent stack inspired by XiangShan. */
class BoomRasStack(implicit p: Parameters) extends RasModule()(p)
    with HasCircularQueuePtrHelper {
  class RasIO extends Bundle {
    val spec = new RasSpecIO
    val commit = new RasCommitIO
    val redirect = new RasRedirectIO
    val meta = Output(new RasInternalMeta)
    val specNearOverflow = Output(Bool())
  }
  val io = IO(new RasIO)

  private val specQueue = RegInit(VecInit(Seq.fill(RasSpecQueueSize)(0.U.asTypeOf(new RasEntry))))
  private val commitStack = RegInit(VecInit(Seq.fill(RasCommitStackSize)(0.U.asTypeOf(new RasEntry))))
  private val specNos = RegInit(VecInit(Seq.fill(RasSpecQueueSize)(
    RasPtr(false.B, 0.U, RasSpecQueueSize))))

  // SSP indexes the speculative top in the committed stack; CSP is its
  // committed counterpart. TOSR is the speculative top, TOSW the next write,
  // and BOS the oldest speculative entry that has not been reclaimed.
  private val ssp = RegInit(0.U(log2Ceil(RasCommitStackSize).W))
  private val csp = RegInit(0.U(log2Ceil(RasCommitStackSize).W))
  private val tosr = RegInit(RasPtr(true.B, (RasSpecQueueSize - 1).U, RasSpecQueueSize))
  private val tosw = RegInit(RasPtr(false.B, 0.U, RasSpecQueueSize))
  private val bos = RegInit(RasPtr(false.B, 0.U, RasSpecQueueSize))
  private val sctr = RegInit(0.U(RasStackCounterWidth.W))
  private val counterMax = ((BigInt(1) << RasStackCounterWidth) - 1).U

  private def ptrInc(ptr: UInt): UInt =
    Mux(ptr === (RasCommitStackSize - 1).U, 0.U, ptr + 1.U)
  private def ptrDec(ptr: UInt): UInt =
    Mux(ptr === 0.U, (RasCommitStackSize - 1).U, ptr - 1.U)

  private def tosrInRange(currTosr: RasPtr, currTosw: RasPtr): Bool =
    !isBefore(currTosr, bos) && isBefore(currTosr, currTosw)

  private def getTop(currSsp: UInt, currTosr: RasPtr, currTosw: RasPtr): RasEntry =
    Mux(tosrInRange(currTosr, currTosw), specQueue(currTosr.value), commitStack(currSsp))

  val specTopEntry = getTop(ssp, tosr, tosw)
  io.spec.popAddr := specTopEntry.retAddr
  io.meta.ssp := ssp
  io.meta.sctr := sctr
  // A reclaimed predecessor must stay outside the live interval as the ring
  // wraps. Otherwise an ancient pointer can alias a newly allocated entry.
  io.meta.tosr := Mux(tosrInRange(tosr, tosw), tosr, bos.dec())
  io.meta.tosw := tosw
  io.meta.nos := specNos(tosr.value)

  // Redirect always restores the checkpoint, including redirects caused by
  // ordinary branches. Its optional RAS action then replaces the prediction.
  val base = Wire(new RasInternalMeta)
  base := Mux(io.redirect.valid, io.redirect.meta, io.meta)
  val pop = Mux(io.redirect.valid, io.redirect.isRet, io.spec.fire && io.spec.popValid)
  val push = Mux(io.redirect.valid, io.redirect.isCall, io.spec.fire && io.spec.pushValid)
  val pushAddr = Mux(io.redirect.valid, io.redirect.callAddr, io.spec.pushAddr)

  // Apply pop before push for the RISC-V PopAndPush hint. Pure pushes retain
  // the old top as their predecessor; pure pops leave TOSW unchanged.
  val afterPop = WireInit(base)
  when(pop) {
    when(tosrInRange(base.tosr, base.tosw)) {
      afterPop.tosr := Mux(tosrInRange(base.nos, base.tosw), base.nos, bos.dec())
    }
    when(base.sctr > 0.U) {
      afterPop.sctr := base.sctr - 1.U
    }.otherwise {
      afterPop.ssp := ptrDec(base.ssp)
      afterPop.sctr := getTop(ptrDec(base.ssp), afterPop.tosr, base.tosw).ctr
    }
  }
  val topAfterPop = getTop(afterPop.ssp, afterPop.tosr, afterPop.tosw)
  val pushCtr = Mux(topAfterPop.retAddr === pushAddr && afterPop.sctr < counterMax,
    afterPop.sctr + 1.U, 0.U)

  when(io.redirect.valid || io.spec.fire) {
    ssp := afterPop.ssp
    sctr := afterPop.sctr
    tosr := afterPop.tosr
    tosw := afterPop.tosw
    when(push) {
      specQueue(afterPop.tosw.value).retAddr := pushAddr
      specQueue(afterPop.tosw.value).ctr := pushCtr
      specNos(afterPop.tosw.value) := afterPop.tosr
      tosr := afterPop.tosw
      tosw := afterPop.tosw.inc()
      sctr := pushCtr
      when(!(topAfterPop.retAddr === pushAddr && afterPop.sctr < counterMax)) {
        ssp := ptrInc(afterPop.ssp)
      }
    }
  }

  // Entries are registers, so a push written at this edge is visible to the
  // next cycle's top/link reads without the delayed-write bypass of an SRAM.
  val commitBase = Mux(io.commit.metaSsp =/= csp, io.commit.metaSsp, csp)
  val commitTop = commitStack(commitBase)
  val commitAfterPop = WireInit(commitBase)
  val commitPopTop = WireInit(commitTop)
  when(io.commit.popValid) {
    when(commitTop.ctr > 0.U) {
      commitPopTop.ctr := commitTop.ctr - 1.U
    }.otherwise {
      commitAfterPop := ptrDec(commitBase)
      commitPopTop := commitStack(ptrDec(commitBase))
    }
  }
  when(io.commit.valid) {
    when(io.commit.popValid) {
      csp := commitAfterPop
      when(commitTop.ctr > 0.U) {
        commitStack(commitBase).ctr := commitTop.ctr - 1.U
      }
    }
    when(io.commit.pushValid) {
      when(commitPopTop.retAddr === io.commit.pushAddr && commitPopTop.ctr < counterMax) {
        commitStack(commitAfterPop).ctr := commitPopTop.ctr + 1.U
        csp := commitAfterPop
      }.otherwise {
        csp := ptrInc(commitAfterPop)
        commitStack(ptrInc(commitAfterPop)).retAddr := io.commit.pushAddr
        commitStack(ptrInc(commitAfterPop)).ctr := 0.U
      }
    }
    when(io.commit.pushValid) {
      bos := io.commit.metaTosw
    }.elsewhen(distanceBetween(io.commit.metaTosw, bos) > 2.U) {
      bos := io.commit.metaTosw.dec()
    }
  }
  io.specNearOverflow := distanceBetween(tosw, bos) > (RasSpecQueueSize - 2).U
}
