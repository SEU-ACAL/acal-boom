package boom.fdip.ifu.bpd.ras

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.EphemeralSimulator._
import circt.stage.ChiselStage
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.{TileKey, TileVisibilityNodeKey}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import boom.fdip.common.{BoomCoreParams, BoomTileParams}
import boom.fdip.ifu.{FDIPFetchTargetQueue, FtqParameters, IFUtoFTQ, FTQToBPUIO}
import boom.fdip.ifu.bpd.{BoomBpdParams, BranchPredictor, BPUToFTQIO}

/** Run with: sbt 'boom/Test/runMain boom.fdip.ifu.bpd.ras.RasWiringCheck'. */
object RasWiringCheck {
  private val baseParameters: Parameters = Parameters.empty.alterPartial {
    case TileKey => BoomTileParams(core = BoomCoreParams(fetchWidth = 4,
      ftq = FtqParameters(4), bpd = BoomBpdParams(ras = BoomRasParams(
        CommitStackSize = 4, SpecQueueSize = 8, StackCounterWidth = 2))))
  }

  // BOOM bundle widths use the tile's negotiated physical address width.
  // Only negotiate this graph; the tests do not instantiate a TileLink bus.
  private class AddressWidths(implicit p: Parameters) extends LazyModule {
    val visibility = TLEphemeralNode()
    val client = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("ras-test")))))
    val manager = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0, BigInt("ffffffff", 16))),
      supportsGet = TransferSizes(1, 8))), beatBytes = 8)))
    manager := visibility := client
    lazy val module = new LazyModuleImp(this) {}
  }
  private val widths = LazyModule(new AddressWidths()(baseParameters))
  private implicit val p: Parameters = baseParameters.alterPartial {
    case TileVisibilityNodeKey => widths.visibility
  }

  private def zeroInputs(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(zeroInputs)
    case v: Vec[_] => v.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf.poke(BigInt(0))
    case _ => ()
  }

  private def tieZero(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(tieZero)
    case v: Vec[_] => v.foreach(tieZero)
    case b: Bool => b := false.B
    case s: SInt => s := 0.S
    case u: UInt => u := 0.U
    case _ => ()
  }

  private case class Snapshot(ssp: BigInt, sctr: BigInt, tosrFlag: Boolean,
    tosr: BigInt, toswFlag: Boolean, tosw: BigInt, nosFlag: Boolean, nos: BigInt, top: BigInt)
  private def readMeta(m: RasMeta): Snapshot = Snapshot(
    m.ssp.peek().litValue, m.sctr.peek().litValue,
    m.tosr.flag.peek().litToBoolean, m.tosr.value.peek().litValue,
    m.tosw.flag.peek().litToBoolean, m.tosw.value.peek().litValue,
    m.nos.flag.peek().litToBoolean, m.nos.value.peek().litValue, m.topRetAddr.peek().litValue)
  private def pokeMeta(m: RasMeta, s: Snapshot): Unit = {
    m.ssp.poke(s.ssp); m.sctr.poke(s.sctr)
    m.tosr.flag.poke(s.tosrFlag); m.tosr.value.poke(s.tosr)
    m.tosw.flag.poke(s.toswFlag); m.tosw.value.poke(s.tosw)
    m.nos.flag.poke(s.nosFlag); m.nos.value.poke(s.nos)
    m.topRetAddr.poke(s.top)
  }

  private def checkStack(): Unit = simulate(new BoomRAS) { d =>
    zeroInputs(d.io)
    def resetStack(): Unit = {
      zeroInputs(d.io)
      d.reset.poke(true); d.clock.step(2); d.reset.poke(false)
    }
    def predict(action: Int, pc: Int, pos: Int = 1): Snapshot = {
      val before = readMeta(d.io.meta)
      d.io.specIn.valid.poke(true)
      d.io.specIn.bits.attribute.branchType.poke(if ((action & 1) != 0) 3 else 2)
      d.io.specIn.bits.attribute.rasAction.poke(action)
      d.io.specIn.bits.startPc.poke(pc); d.io.specIn.bits.cfiPosition.poke(pos)
      d.clock.step(); d.io.specIn.valid.poke(false)
      before
    }
    def commit(s: Snapshot, action: Int, pc: Int, pos: Int = 1): Unit = {
      d.io.commit.valid.poke(true)
      d.io.commit.bits.cfiValid.poke(action != 0); d.io.commit.bits.taken.poke(true)
      d.io.commit.bits.attribute.rasAction.poke(action)
      d.io.commit.bits.startPc.poke(pc); d.io.commit.bits.cfiPosition.poke(pos)
      d.io.commit.bits.meta.ssp.poke(s.ssp)
      d.io.commit.bits.meta.tosw.flag.poke(s.toswFlag)
      d.io.commit.bits.meta.tosw.value.poke(s.tosw)
      d.clock.step(); d.io.commit.valid.poke(false)
    }
    def redirect(s: Snapshot, action: Int, pc: Int, taken: Boolean = true): Unit = {
      pokeMeta(d.io.redirect.bits.rasMeta, s)
      d.io.redirect.valid.poke(true)
      d.io.redirect.bits.cfiValid.poke(true); d.io.redirect.bits.taken.poke(taken)
      d.io.redirect.bits.attribute.rasAction.poke(action)
      d.io.redirect.bits.startPc.poke(pc); d.io.redirect.bits.cfiPosition.poke(1)
      d.clock.step(); d.io.redirect.valid.poke(false)
    }

    resetStack()
    val empty = predict(2, 0x1000)
    d.io.topRetAddr.expect(BigInt(0x1004))
    val beforeB = predict(2, 0x2000)
    d.io.topRetAddr.expect(BigInt(0x2004))
    d.clock.step(3)
    d.io.topRetAddr.expect(BigInt(0x2004))
    // Recovery on a non-RAS branch must undo a wrong-path call. It also
    // outranks a simultaneous prediction of another call.
    d.io.specIn.valid.poke(true)
    d.io.specIn.bits.attribute.rasAction.poke(2)
    d.io.specIn.bits.startPc.poke(0x4000)
    redirect(beforeB, 0, 0x2000)
    d.io.specIn.valid.poke(false)
    assert(readMeta(d.io.meta) == beforeB)
    redirect(beforeB, 2, 0x3000)
    d.io.topRetAddr.expect(BigInt(0x3004))
    redirect(beforeB, 1, 0x3000)
    d.io.topRetAddr.expect(BigInt(0))
    redirect(beforeB, 2, 0x3000, taken = false)
    assert(readMeta(d.io.meta) == beforeB)
    redirect(empty, 2, 0x1000)
    d.io.topRetAddr.expect(BigInt(0x1004))

    resetStack()
    val recursion = (0 until 3).map(_ => predict(2, 0x1000))
    d.io.meta.sctr.expect(BigInt(2))
    recursion.foreach(s => commit(s, 2, 0x1000))
    for (_ <- 0 until 3) {
      val before = predict(1, 0)
      commit(before, 1, 0)
    }
    d.io.topRetAddr.expect(BigInt(0))

    resetStack()
    val a = predict(2, 0x1000); commit(a, 2, 0x1000)
    val b = predict(2, 0x2000); commit(b, 2, 0x2000)
    d.io.topRetAddr.expect(BigInt(0x2004)) // target before PopAndPush
    val coroutine = predict(3, 0x3000)
    d.io.topRetAddr.expect(BigInt(0x3004))
    commit(coroutine, 3, 0x3000)
    predict(1, 0)
    d.io.topRetAddr.expect(BigInt(0x1004))

    resetStack()
    // Exercise pointer wrap while commit keeps the speculative queue short.
    for (i <- 0 until 24) {
      val call = predict(2, 0x1000 + 8 * i); commit(call, 2, 0x1000 + 8 * i)
      d.io.topRetAddr.expect(BigInt(0x1004 + 8 * i))
      val ret = predict(1, 0); commit(ret, 1, 0)
      d.io.topRetAddr.expect(BigInt(0))
    }

    resetStack()
    val calls = (0 until 7).map(i => (predict(2, 0x1000 + i * 8), 0x1000 + i * 8))
    d.io.specNearOverflow.expect(true.B)
    val full = readMeta(d.io.meta)
    predict(2, 0x4000)
    assert(readMeta(d.io.meta) == full)
    calls.take(3).foreach { case (s, pc) => commit(s, 2, pc) }
    d.io.specNearOverflow.expect(false.B)
    predict(2, 0x5000)
    d.io.topRetAddr.expect(BigInt(0x5004))
    println("RAS prediction/recovery/commit/PopAndPush/wrap/overflow checks passed")
  }

  class FtqHarness extends Module {
    val io = IO(new Bundle {
      val fromBpu = Flipped(new BPUToFTQIO)
      val fromIfu = Flipped(new IFUtoFTQ)
      val fromBackend = new Bundle {
        val redirect_val = Input(Bool())
        val redirect_ftq_idx = Input(UInt(3.W))
        val deq = Flipped(chisel3.util.Valid(UInt(3.W)))
        val brupdate = new Bundle {
          val b2 = Input(new Bundle {
            val mispredict = Bool()
            val taken = Bool()
            val is_ret = Bool()
            val cfi_type = UInt(2.W)
          })
        }
      }
      val toBPU = new FTQToBPUIO
    })
    val ftq = Module(new FDIPFetchTargetQueue)
    ftq.io.fromBpu <> io.fromBpu
    ftq.io.fromIfu <> io.fromIfu
    io.toBPU <> ftq.io.toBPU
    tieZero(ftq.io.fromBackend)
    // This unbounded SInt is inferred from the branch unit in a real core.
    ftq.io.fromBackend.brupdate.b2.target_offset := 0.S(40.W)
    ftq.io.fromBackend.redirect_val := io.fromBackend.redirect_val
    ftq.io.fromBackend.redirect_ftq_idx := io.fromBackend.redirect_ftq_idx
    ftq.io.fromBackend.deq := io.fromBackend.deq
    ftq.io.fromBackend.brupdate.b2.mispredict := io.fromBackend.brupdate.b2.mispredict
    ftq.io.fromBackend.brupdate.b2.taken := io.fromBackend.brupdate.b2.taken
    ftq.io.fromBackend.brupdate.b2.is_ret := io.fromBackend.brupdate.b2.is_ret
    ftq.io.fromBackend.brupdate.b2.cfi_type := io.fromBackend.brupdate.b2.cfi_type
    ftq.io.toBackend.get_ftq_pc.foreach(_.ftq_idx := 0.U)
    ftq.io.toICache.req.ready := false.B
    ftq.io.toPrefetch.ready := false.B
    ftq.io.toIfu.req.ready := false.B
    ftq.io.elements.get("main_wait_waylookup").foreach(tieZero)
    ftq.io.debug_ftq_idx.foreach(_ := 0.U)
  }

  private def checkFtq(): Unit = simulate(new FtqHarness) { d =>
    zeroInputs(d.io)
    d.reset.poke(true); d.clock.step(2); d.reset.poke(false)
    val s = Snapshot(2, 1, false, 3, false, 4, false, 2, 0x1234)
    d.io.fromBpu.resp.valid.poke(true)
    d.io.fromBpu.resp.bits.f1.valid.poke(true)
    d.io.fromBpu.resp.bits.f1.pc.poke(0x1000)
    d.io.fromBpu.resp.bits.f2.valid.poke(true)
    d.io.fromBpu.resp.bits.f2.ftq_idx.poke(0)
    pokeMeta(d.io.fromBpu.resp.bits.f2.rasMeta, s)
    d.clock.step()
    d.io.fromBpu.resp.valid.poke(false)
    d.io.fromBpu.resp.bits.f1.valid.poke(false)
    d.io.fromBpu.resp.bits.f2.valid.poke(false)

    d.io.fromIfu.redirect_from_fetch.poke(true)
    d.io.fromIfu.redirect_ftq_idx.poke(0)
    d.io.fromIfu.predecode_infos.valid.poke(true)
    d.io.fromIfu.predecode_infos.bits.ftq_idx.poke(0)
    d.io.fromIfu.predecode_infos.bits.br_infos.cfiPosition.valid.poke(true)
    d.io.fromIfu.predecode_infos.bits.br_infos.cfiPosition.bits.poke(1)
    d.io.fromIfu.predecode_infos.bits.br_infos.cfi_taken.poke(true)
    d.io.fromIfu.predecode_infos.bits.br_infos.attribute.branchType.poke(1)
    d.clock.step()
    d.io.toBPU.ras_updateInfo.valid.expect(true.B)
    assert(readMeta(d.io.toBPU.ras_updateInfo.bits.rasMeta) == s)
    d.io.toBPU.ras_updateInfo.bits.attribute.rasAction.expect(BigInt(0))
    d.io.fromIfu.redirect_from_fetch.poke(false)
    d.io.fromIfu.predecode_infos.valid.poke(false)
    d.clock.step()

    // The backend's resolved return action replaces the old pd_mem action.
    d.io.fromBackend.redirect_val.poke(true)
    d.io.fromBackend.redirect_ftq_idx.poke(0)
    d.io.fromBackend.brupdate.b2.mispredict.poke(true)
    d.io.fromBackend.brupdate.b2.taken.poke(true)
    d.io.fromBackend.brupdate.b2.is_ret.poke(true)
    d.io.fromBackend.brupdate.b2.cfi_type.poke(3)
    d.clock.step()
    d.io.toBPU.ras_updateInfo.valid.expect(true.B)
    d.io.toBPU.ras_updateInfo.bits.attribute.rasAction.expect(BigInt(1))
    assert(readMeta(d.io.toBPU.ras_updateInfo.bits.rasMeta) == s)
    d.io.fromBackend.redirect_val.poke(false)
    d.io.fromBackend.brupdate.b2.mispredict.poke(false)
    d.clock.step()

    // An entry without a CFI still sends reclamation metadata at retirement.
    d.io.fromIfu.predecode_infos.valid.poke(true)
    d.io.fromIfu.predecode_infos.bits.br_infos.cfiPosition.valid.poke(false)
    d.clock.step(); d.io.fromIfu.predecode_infos.valid.poke(false)
    d.io.fromBackend.deq.valid.poke(true); d.io.fromBackend.deq.bits.poke(1)
    d.clock.step(); d.io.fromBackend.deq.valid.poke(false)
    d.clock.step()
    d.io.toBPU.ras_commitInfo.valid.expect(true.B)
    d.io.toBPU.ras_commitInfo.bits.meta.ssp.expect(s.ssp)
    d.io.toBPU.ras_commitInfo.bits.meta.tosw.value.expect(s.tosw)
    d.io.toBPU.ras_commitInfo.bits.cfiValid.expect(false.B)
    d.io.toBPU.train.valid.expect(false.B)
    println("FTQ checkpoint/IFU redirect/backend resolution/retirement checks passed")
  }

  class IntegrationHarness(implicit p: Parameters) extends Module {
    val io = IO(new Bundle { val ftqFull = Output(Bool()) })
    val ftq = Module(new FDIPFetchTargetQueue)
    val bpu = Module(new BranchPredictor)
    bpu.io.fromFtq <> ftq.io.toBPU
    ftq.io.fromBpu <> bpu.io.toFtq
    bpu.io.reset_vector := 0x80000000L.U
    bpu.io.f3_fire := false.B
    tieZero(ftq.io.fromBackend)
    ftq.io.fromBackend.brupdate.b2.target_offset := 0.S(40.W)
    ftq.io.fromIfu := 0.U.asTypeOf(ftq.io.fromIfu)
    ftq.io.toBackend.get_ftq_pc.foreach(_.ftq_idx := 0.U)
    ftq.io.toICache.req.ready := false.B
    ftq.io.toPrefetch.ready := false.B
    ftq.io.toIfu.req.ready := false.B
    ftq.io.elements.get("main_wait_waylookup").foreach(tieZero)
    ftq.io.debug_ftq_idx.foreach(_ := 0.U)
    io.ftqFull := ftq.io.ftq_full
  }

  def main(args: Array[String]): Unit = {
    checkStack()
    checkFtq()
    val defaultParameters = p.alterPartial {
      case TileKey => BoomTileParams(core = BoomCoreParams(fetchWidth = 4))
    }
    val rtl = ChiselStage.emitSystemVerilog(new IntegrationHarness()(defaultParameters))
    val out = java.nio.file.Files.createTempDirectory("fdip-ras-wiring-rtl")
    val rtlPath = out.resolve("IntegrationHarness.sv")
    java.nio.file.Files.write(rtlPath, rtl.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    println(s"BPU + FTQ + RAS integration RTL elaboration passed: $rtlPath")
  }
}
