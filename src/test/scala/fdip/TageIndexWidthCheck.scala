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
import boom.fdip.ifu.bpd.BoomBpdParams

/** Run with: sbt 'boom/Test/runMain boom.fdip.ifu.bpd.tage.TageIndexWidthCheck'. */
object TageIndexWidthCheck {
  private val checkedRows = Seq(512, 1024, 2048)
  private val testTageParams = BoomTageParams(tableInfo = Seq(
    (512, 4, 10), (512, 8, 10), (1024, 13, 10),
    (2048, 16, 10), (512, 32, 10), (512, 119, 10)))
  private val baseParameters = Parameters.empty.alterPartial {
    case TileKey => BoomTileParams(core = BoomCoreParams(fetchWidth = 8,
      globalHistoryLength = 119, bpd = BoomBpdParams(tage = testTageParams)))
  }
  private class AddressWidths(implicit p: Parameters) extends LazyModule {
    val visibility = TLEphemeralNode()
    val client = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("tage-index-test")))))
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
      val write = Input(MixedVec(checkedRows.map(n => Valid(new TageTableWriteReq(n)))))
      val read = Input(MixedVec(checkedRows.map(n => Vec(TageNways, Valid(new TageTableReadReq(n))))))
      val response = Output(Vec(checkedRows.size, Vec(TageNways, new TageTableReadResp)))
      val providerCtr = Output(UInt(3.W))
    })
    probe.providerCtr := t0_providerCtr(0)
    for ((nRows, i) <- checkedRows.zipWithIndex) {
      val table = Module(new TageTable(nRows, 10, 4, params.uBitPeriod))
      require(table.io.readReq.head.bits.setIdx.getWidth == log2Ceil(nRows))
      require(table.io.writeReq.bits.setIdx.getWidth == log2Ceil(nRows))
      table.io.writeReq := probe.write(i)
      table.io.readReq := probe.read(i)
      probe.response(i) := table.io.readResp
    }
    for ((info, i) <- params.tableInfo.zipWithIndex) {
      require(tables(i).io.readReq.head.bits.setIdx.getWidth == log2Ceil(info._1))
      require(tables(i).io.writeReq.bits.setIdx.getWidth == log2Ceil(info._1))
      require(ctrWriteBypass.head.idx(i).head.getWidth == log2Ceil(info._1))
    }
  }

  private def zeroInputs(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(zeroInputs)
    case v: Vec[_] => v.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf.poke(BigInt(0))
    case _ => ()
  }

  def main(args: Array[String]): Unit = simulate(new Probe) { d =>
    zeroInputs(d.io)
    zeroInputs(d.probe)
    d.reset.poke(true); d.clock.step(2); d.reset.poke(false)
    d.clock.step(checkedRows.max + 2)

    val addresses = Seq(Seq(226, 482), Seq(226, 738), Seq(226, 738, 1762))
    for ((indexes, table) <- addresses.zipWithIndex; (idx, value) <- indexes.zipWithIndex) {
      val write = d.probe.write(table)
      write.valid.poke(true)
      write.bits.setIdx.poke(idx)
      for (way <- 0 until d.TageNways) {
        write.bits.wayMask(way).poke(true)
        write.bits.writeEntry(way).poke(true)
        write.bits.writeUseful(way).poke(true)
        write.bits.entry(way).valid.poke(true)
        write.bits.entry(way).tag.poke(341)
        write.bits.entry(way).ctr.poke(value + way + 1)
        write.bits.u(way).poke(value + 1)
      }
      d.clock.step()
      write.valid.poke(false)
    }
    for ((indexes, table) <- addresses.zipWithIndex; (idx, value) <- indexes.zipWithIndex) {
      for (way <- 0 until d.TageNways) {
        d.probe.read(table)(way).valid.poke(true)
        d.probe.read(table)(way).bits.setIdx.poke(idx)
      }
      d.clock.step()
      for (way <- 0 until d.TageNways) {
        d.probe.response(table)(way).valid.expect(true.B)
        d.probe.response(table)(way).tag.expect(341)
        d.probe.response(table)(way).ctr.expect(value + way + 1)
        d.probe.response(table)(way).u.expect(value + 1)
      }
    }

    // These rows share their low 9 bits and tag when PHR=0.
    // The bypass must distinguish them using the table's full index.
    def setup(index: Int, way: Int = 0, provider: Int = 2): Unit = {
      zeroInputs(d.io)
      d.io.train.valid.poke(true)
      val train = d.io.train.bits
      train.startPc.poke(BigInt("80000000", 16) + index * 2)
      train.cfi.valid.poke(true)
      train.cfi.position.poke(7)
      train.cfi.taken.poke(true)
      train.cfi.attribute.branchType.poke(BranchAttribute.BranchType.Conditional.litValue)
      train.btbMeta(0).valid.poke(true)
      train.btbMeta(0).bits.hit.poke(true)
      train.btbMeta(0).bits.position.poke(7)
      train.btbMeta(0).bits.attribute.branchType.poke(BranchAttribute.BranchType.Conditional.litValue)
      train.tageMeta.provider(0).valid.poke(true)
      train.tageMeta.provider(0).bits.poke(provider)
      train.tageMeta.provider_wayMask(0).poke(1 << way)
      train.tageMeta.provider_ctr(0).poke(4)
    }
    for ((provider, indexes) <- Seq(2 -> Seq(226, 738), 3 -> Seq(226, 738, 1762))) {
      zeroInputs(d.io)
      d.reset.poke(true); d.clock.step(2); d.reset.poke(false)
      d.clock.step(checkedRows.max + 2)
      for (index <- indexes) {
        setup(index, provider = provider)
        d.probe.providerCtr.expect(4); d.clock.step()
      }
      for (index <- indexes) {
        setup(index, provider = provider); d.probe.providerCtr.expect(5)
      }
      setup(indexes.last, provider = provider); d.clock.step()
      d.probe.providerCtr.expect(6)
      for (index <- indexes.init) {
        setup(index, provider = provider); d.probe.providerCtr.expect(5)
      }
      setup(indexes.last, way = 1, provider = provider); d.probe.providerCtr.expect(4)
      setup(indexes.last, provider = 1); d.probe.providerCtr.expect(4)
    }
    println("PASS: per-table 9/10/11-bit SRAM indexes and full-index bypass identity")
  }
}
