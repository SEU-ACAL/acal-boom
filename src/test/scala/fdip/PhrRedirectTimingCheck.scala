package boom.fdip.ifu

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.EphemeralSimulator._
import circt.stage.ChiselStage
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.{TileKey, TileVisibilityNodeKey}
import freechips.rocketchip.tilelink._
import boom.fdip.common.{BoomCoreParams, BoomTileParams}
import boom.fdip.ifu.bpd.{BPUToFTQIO, PHR}

/** sbt 'boom/Test/runMain boom.fdip.ifu.PhrRedirectTimingCheck <rtl-output-dir>' */
object PhrRedirectTimingCheck {
  private val baseParameters = Parameters.empty.alterPartial {
    case TileKey => BoomTileParams(core = BoomCoreParams(fetchWidth = 8, globalHistoryLength = 119,
      ftq = FtqParameters(4)))
  }

  private class AddressWidths(implicit p: Parameters) extends LazyModule {
    val visibility = TLEphemeralNode()
    val client = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("phr-test")))))
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

  private def tieZero(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(tieZero)
    case v: Vec[_] => v.foreach(tieZero)
    case b: Bool => b := false.B
    case s: SInt => s := 0.S
    case u: UInt => u := 0.U
    case _ => ()
  }

  private def zeroInputs(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(zeroInputs)
    case v: Vec[_] => v.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf.poke(BigInt(0))
    case _ => ()
  }

  // A test-only tap observes the live pointer without altering production IO.
  private class ObservedPHR extends PHR {
    val pointer = IO(Output(UInt(ptr.value.getWidth.W)))
    pointer := ptr.value
  }

  private class Harness extends Module {
    val io = IO(new Bundle {
      val fromBpu = Flipped(new BPUToFTQIO)
      val fromIfu = Flipped(new IFUtoFTQ)
      val backendValid = Input(Bool())
      val backendMispredict = Input(Bool())
      val backendTaken = Input(Bool())
      val backendTarget = Input(UInt(40.W))
      val toBpu = new FTQToBPUIO
      val phrPointer = Output(UInt(8.W))
      val phrHistory = Output(UInt(119.W))
    })
    val ftq = Module(new FDIPFetchTargetQueue)
    val phr = Module(new ObservedPHR)
    ftq.io.fromBpu <> io.fromBpu
    ftq.io.fromIfu <> io.fromIfu
    io.toBpu <> ftq.io.toBPU
    tieZero(ftq.io.fromBackend)
    ftq.io.fromBackend.brupdate.b2.target_offset := 0.S(40.W)
    ftq.io.fromBackend.redirect_val := io.backendValid
    ftq.io.fromBackend.redirect_ftq_idx := 0.U
    ftq.io.fromBackend.redirect_pc := io.backendTarget
    ftq.io.fromBackend.brupdate.b2.mispredict := io.backendMispredict
    ftq.io.fromBackend.brupdate.b2.taken := io.backendTaken
    ftq.io.fromBackend.brupdate.b2.cfi_type := 1.U // CFI_BR
    ftq.io.fromBackend.brupdate.b2.uop.is_rvc := true.B
    ftq.io.fromBackend.brupdate.b2.uop.pc_lob := 6.U
    ftq.io.toBackend.get_ftq_pc.foreach(_.ftq_idx := 0.U)
    ftq.io.toICache.req.ready := false.B
    ftq.io.toPrefetch.ready := false.B
    ftq.io.toIfu.req.ready := false.B
    ftq.io.main_wait_waylookup := false.B
    ftq.io.debug_ftq_idx.foreach(_ := 0.U)
    phr.io.s0_fire := false.B
    phr.io.s1_fire := false.B
    phr.io.s2_fire := false.B
    tieZero(phr.io.s1Train)
    tieZero(phr.io.train)
    phr.io.train.redirect := ftq.io.toBPU.redirect
    io.phrPointer := phr.pointer
    io.phrHistory := phr.io.s0_phr
  }

  private def checkTiming(): Unit = simulate(new Harness) { d =>
    zeroInputs(d.io)
    d.reset.poke(true); d.clock.step(2); d.reset.poke(false)

    val blockPc = BigInt("80000310", 16)
    val checkpointPtr = 49
    val checkpointLow = BigInt(0x155)
    // Seed an FTQ slot and its prediction checkpoint through real BPU IO.
    d.io.fromBpu.resp.valid.poke(true)
    d.io.fromBpu.resp.bits.f1.valid.poke(true)
    d.io.fromBpu.resp.bits.f1.pc.poke(blockPc)
    d.io.fromBpu.resp.bits.f2.valid.poke(true)
    d.io.fromBpu.resp.bits.f2.ftq_idx.poke(0)
    d.io.fromBpu.resp.bits.f2.phrMeta.ptr.value.poke(checkpointPtr)
    d.io.fromBpu.resp.bits.f2.phrMeta.phrLowBits.poke(checkpointLow)
    d.clock.step()
    d.io.fromBpu.resp.valid.poke(false)
    d.io.fromBpu.resp.bits.f1.valid.poke(false)
    d.io.fromBpu.resp.bits.f2.valid.poke(false)
    d.clock.step(2)

    def ifuEvent(taken: Boolean, position: Int, target: BigInt): Unit = {
      d.io.fromIfu.redirect_from_fetch.poke(true)
      d.io.fromIfu.redirect_ftq_idx.poke(0)
      d.io.fromIfu.redirect_target.poke(target)
      d.io.fromIfu.predecode_infos.valid.poke(true)
      d.io.fromIfu.predecode_infos.bits.ftq_idx.poke(0)
      d.io.fromIfu.predecode_infos.bits.br_infos.cfiPosition.valid.poke(taken)
      d.io.fromIfu.predecode_infos.bits.br_infos.cfiPosition.bits.poke(position)
      d.io.fromIfu.predecode_infos.bits.br_infos.cfi_taken.poke(taken)
    }
    def expectRedirect(taken: Boolean, cfiPc: BigInt, target: BigInt): Unit = {
      d.io.toBpu.redirect.valid.expect(true.B)
      d.io.toBpu.redirect_val.expect(true.B)
      d.io.toBpu.redirect_ftq_idx.expect(0.U)
      d.io.toBpu.redirect_pc.expect(target.U)
      d.io.toBpu.redirect.bits.cfiPc.expect(cfiPc.U)
      d.io.toBpu.redirect.bits.target.expect(target.U)
      d.io.toBpu.redirect.bits.taken.expect(taken.B)
      d.io.toBpu.redirect.bits.phrMeta.ptr.value.expect(checkpointPtr.U)
      d.io.toBpu.redirect.bits.phrMeta.phrLowBits.expect(checkpointLow.U)
      // Check the history PHR will use at the next edge, including its bypass.
      val hash = (((cfiPc >> 1) & 0x1ff) << 4) ^ ((target >> 2) & 0x7fff)
      val hashHigh = (hash >> 2) & 0x3ff
      val low = if (taken) ((checkpointLow ^ hashHigh) << 2) | (hash & 3) else checkpointLow
      val mask = if (taken) BigInt(0xfff) else BigInt(0x3ff)
      assert((d.io.phrHistory.peek().litValue & mask) == low)
    }

    // The original nsichneu JAL after a bubble: old code returns taken=false.
    val jalTarget = BigInt("800030a8", 16)
    ifuEvent(true, 5, jalTarget)
    d.clock.step()
    expectRedirect(true, blockPc + 10, jalTarget)
    d.io.phrPointer.expect(0.U)

    // Back-to-back opposite direction must not inherit the previous JAL.
    ifuEvent(false, 2, blockPc + 16)
    d.clock.step()
    expectRedirect(false, blockPc + 4, blockPc + 16)
    d.io.phrPointer.expect((checkpointPtr - 2).U)

    ifuEvent(true, 7, BigInt("80004000", 16))
    d.clock.step()
    expectRedirect(true, blockPc + 14, BigInt("80004000", 16))
    d.io.phrPointer.expect(checkpointPtr.U)

    // Backend direction and target win when both sources redirect together.
    d.io.backendValid.poke(true); d.io.backendMispredict.poke(true)
    d.io.backendTaken.poke(false); d.io.backendTarget.poke(BigInt("80005000", 16))
    d.clock.step()
    expectRedirect(false, blockPc + 6, BigInt("80005000", 16))
    d.io.phrPointer.expect((checkpointPtr - 2).U)
    ifuEvent(false, 2, blockPc + 16)
    d.io.backendTaken.poke(true); d.io.backendTarget.poke(BigInt("80006000", 16))
    d.clock.step()
    expectRedirect(true, blockPc + 6, BigInt("80006000", 16))
    d.io.phrPointer.expect(checkpointPtr.U)

    d.io.backendValid.poke(false); d.io.backendMispredict.poke(false)
    d.io.fromIfu.redirect_from_fetch.poke(false)
    d.io.fromIfu.predecode_infos.valid.poke(false)
    d.clock.step()
    d.io.toBpu.redirect.valid.expect(false.B)
    d.io.phrPointer.expect((checkpointPtr - 2).U)
    d.clock.step()
    d.io.phrPointer.expect((checkpointPtr - 2).U)
    println("FTQ -> PHR redirect timing, payload, taken replay, NT restore and backend priority passed")
  }

  def main(args: Array[String]): Unit = {
    checkTiming()
    val out = if (args.nonEmpty) java.nio.file.Paths.get(args(0))
      else java.nio.file.Files.createTempDirectory("fdip-phr-redirect-rtl")
    java.nio.file.Files.createDirectories(out)
    val rtlPath = out.resolve("PhrRedirectTimingHarness.sv")
    java.nio.file.Files.write(rtlPath,
      ChiselStage.emitSystemVerilog(new Harness).getBytes(java.nio.charset.StandardCharsets.UTF_8))
    println(s"Regenerated FTQ + PHR RTL: $rtlPath")
  }
}
