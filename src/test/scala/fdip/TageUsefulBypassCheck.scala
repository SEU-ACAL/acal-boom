package boom.fdip.ifu.bpd.tage

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.EphemeralSimulator._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.{TileKey, TileVisibilityNodeKey}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import boom.fdip.common.{BoomCoreParams, BoomTileParams}
import boom.fdip.ifu.BranchAttribute

/** Run with: sbt 'boom/Test/runMain boom.fdip.ifu.bpd.tage.TageUsefulBypassCheck'. */
object TageUsefulBypassCheck {
  private val baseParameters = Parameters.empty.alterPartial {
    case TileKey => BoomTileParams(core = BoomCoreParams(fetchWidth = 8,
      globalHistoryLength = 119))
  }
  private class AddressWidths(implicit p: Parameters) extends LazyModule {
    val visibility = TLEphemeralNode()
    val client = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("tage-test")))))
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

  private class Probe extends TageBranchPredictor {
    val probe = IO(new Bundle {
      val providerU = Output(Vec(2, UInt(2.W)))
      val entryWrite = Output(Vec(TageNways, Bool()))
      val usefulWrite = Output(Vec(TageNways, Bool()))
      val u = Output(Vec(TageNways, UInt(2.W)))
      val clear = Output(Valid(UInt(10.W)))
      val allocateWay = Output(UInt(TageNways.W))
    })
    probe.providerU := VecInit(t0_providerU.take(2))
    probe.entryWrite := tables.head.io.writeEntryAccepted
    probe.usefulWrite := tables.head.io.writeUsefulAccepted
    probe.u := tables.head.io.writeReq.bits.u
    probe.clear := tables.head.io.clearUseful
    probe.allocateWay := t0_allocateWayMask
  }

  private def zeroInputs(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(zeroInputs)
    case v: Vec[_] => v.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf.poke(BigInt(0))
    case _ => ()
  }

  def main(args: Array[String]): Unit = simulate(new Probe) { d =>
    def resetPredictor(): Unit = {
      zeroInputs(d.io)
      d.reset.poke(true); d.clock.step(2); d.reset.poke(false)
      d.clock.step(1024)
    }
    def setup(pc: BigInt = BigInt("80000000", 16), way: Int = 0, u: Int = 0,
      useful: Boolean = true, position: Int = 1): Unit = {
      zeroInputs(d.io)
      d.io.train.valid.poke(true)
      val train = d.io.train.bits
      train.startPc.poke(pc)
      train.cfi.valid.poke(true)
      train.cfi.position.poke(position)
      train.cfi.taken.poke(true)
      train.cfi.attribute.branchType.poke(BranchAttribute.BranchType.Conditional.litValue)
      train.btbMeta(0).valid.poke(true)
      train.btbMeta(0).bits.hit.poke(true)
      train.btbMeta(0).bits.position.poke(position)
      train.btbMeta(0).bits.attribute.branchType.poke(BranchAttribute.BranchType.Conditional.litValue)
      train.tageMeta.provider(0).valid.poke(true)
      train.tageMeta.provider(0).bits.poke(0)
      train.tageMeta.provider_wayMask(0).poke(1 << way)
      train.tageMeta.provider_ctr(0).poke(4)
      train.tageMeta.provider_u(0).poke(u)
      train.tageMeta.alt_differs(0).poke(useful)
    }
    def update(way: Int, expected: Int): Unit = {
      d.probe.usefulWrite(way).expect(true.B)
      d.probe.u(way).expect(expected)
      d.clock.step()
    }
    def waitForClear(): Unit = {
      d.io.train.valid.poke(false)
      var cycles = 0
      while (!d.probe.clear.valid.peek().litToBoolean && cycles < 2048) {
        d.clock.step(); cycles += 1
      }
      assert(cycles < 2048, "periodic U clear did not occur")
      d.probe.clear.bits.expect(1)
    }

    resetPredictor()
    setup()
    update(0, 1); update(0, 2); update(0, 3)
    d.probe.providerU(0).expect(3)
    d.probe.usefulWrite(0).expect(false.B)
    d.probe.entryWrite(0).expect(false.B)

    resetPredictor()
    setup(); update(0, 1)
    setup(useful = false)
    d.probe.entryWrite(0).expect(true.B)
    d.probe.usefulWrite(0).expect(false.B)
    d.clock.step()
    setup(); update(0, 2); update(0, 3)
    d.io.train.valid.poke(false); d.clock.step(20)
    d.probe.providerU(0).expect(3)

    resetPredictor()
    setup(); update(0, 1)
    setup(way = 1); d.probe.providerU(0).expect(0); update(1, 1)
    setup(); d.probe.providerU(0).expect(1); update(0, 2)
    setup(pc = BigInt("80000800", 16))
    d.probe.providerU(0).expect(0); update(0, 1)
    setup(pc = BigInt("80000010", 16))
    d.probe.providerU(0).expect(0); update(0, 1)
    setup(); d.io.train.bits.tageMeta.provider(0).bits.poke(1)
    d.probe.providerU(0).expect(0)

    resetPredictor()
    setup()
    d.io.train.bits.btbMeta(0).valid.poke(false)
    d.io.train.bits.tageMeta.provider(0).valid.poke(false)
    d.io.train.bits.cfi.mispredict.poke(true)
    val allocatedWay = if (d.probe.allocateWay.peek().litValue == 1) 0 else 1
    update(allocatedWay, 0)
    setup(way = allocatedWay, u = 3)
    d.probe.providerU(0).expect(0); update(allocatedWay, 1)

    resetPredictor()
    setup(position = 3)
    d.io.train.bits.btbMeta(0).bits.position.poke(1)
    d.io.train.bits.tageMeta.provider_ctr(0).poke(3)
    val train = d.io.train.bits
    train.btbMeta(1).valid.poke(true)
    train.btbMeta(1).bits.hit.poke(true)
    train.btbMeta(1).bits.position.poke(3)
    train.btbMeta(1).bits.attribute.branchType.poke(BranchAttribute.BranchType.Conditional.litValue)
    train.tageMeta.provider(1).valid.poke(true)
    train.tageMeta.provider_wayMask(1).poke(2)
    train.tageMeta.provider_ctr(1).poke(4)
    train.tageMeta.alt_differs(1).poke(true)
    d.probe.u(0).expect(1); d.probe.u(1).expect(1); d.clock.step()
    d.probe.providerU(0).expect(1); d.probe.providerU(1).expect(1)
    d.probe.u(0).expect(2); d.probe.u(1).expect(2)

    // The first periodic clear after reset targets row 1. Exercise both
    // an idle FIFO and a simultaneous write that shifts the cleared record.
    for (shift <- Seq(false, true)) {
      resetPredictor()
      setup(pc = BigInt("80000002", 16)); update(0, 1); update(0, 2)
      waitForClear()
      if (shift) {
        setup(pc = BigInt("80000010", 16), way = 1, useful = false)
        d.probe.entryWrite(1).expect(true.B)
      }
      d.clock.step()
      setup(pc = BigInt("80000002", 16), u = 2)
      d.probe.providerU(0).expect(0); update(0, 1)
    }

    resetPredictor()
    setup(); update(0, 1)
    waitForClear()
    setup()
    d.probe.u(0).expect(2)
    d.probe.entryWrite(0).expect(true.B)
    d.probe.usefulWrite(0).expect(false.B)
    d.clock.step()
    d.probe.providerU(0).expect(1); update(0, 2)

    resetPredictor()
    setup(pc = BigInt("80000002", 16)); update(0, 1)
    waitForClear()
    setup(pc = BigInt("80000002", 16))
    d.probe.entryWrite(0).expect(true.B)
    d.probe.usefulWrite(0).expect(false.B)
    d.clock.step()
    d.probe.providerU(0).expect(0); update(0, 1)

    zeroInputs(d.io)
    d.reset.poke(true); d.clock.step(2); d.reset.poke(false)
    setup()
    d.probe.entryWrite(0).expect(false.B)
    d.probe.usefulWrite(0).expect(false.B)
    d.clock.step()
    d.io.train.valid.poke(false); d.clock.step(1023)
    setup()
    d.probe.providerU(0).expect(0); update(0, 1)
    println("PASS: TAGE U accumulation, saturation, counter-only writes, idle, identity, ways, allocation, parallel writes, aging and rejected writes")
  }
}
