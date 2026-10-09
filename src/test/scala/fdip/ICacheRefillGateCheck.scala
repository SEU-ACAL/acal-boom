package boom.fdip.ifu

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.EphemeralSimulator._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.ICacheParams
import freechips.rocketchip.tile.{TileKey, TileVisibilityNodeKey}
import freechips.rocketchip.tilelink._
import boom.fdip.common.{BoomCoreParams, BoomTileParams}

/** Run with: sbt 'boom/Test/runMain boom.fdip.ifu.ICacheRefillGateCheck'. */
object ICacheRefillGateCheck {
  private val cacheParams = ICacheParams(nSets = 64, nWays = 2,
    fetchBytes = 16, blockBytes = 64)
  private val parameters = Parameters.empty.alterPartial {
    case TileKey => BoomTileParams(core = BoomCoreParams(fetchWidth = 8),
      icache = Some(cacheParams))
  }

  private class TestICache(implicit p: Parameters) extends ICache(cacheParams, 0) {
    override lazy val module = new TestICacheModule(this)
  }

  private class TestICacheModule(outer: TestICache) extends ICacheLazyModule(outer) {
    val probe = IO(Output(new Bundle {
      val miss_pending = Bool()
      val refill_write = Bool()
      val way_enq = Bool()
      val way_mask = UInt(outer.icacheParams.nWays.W)
    }))
    probe.miss_pending := missUnit.io.refill_busy
    probe.refill_write := refillWrite
    probe.way_enq := wayLookup.io.write.fire
    probe.way_mask := wayLookup.io.write.bits.entry.waymask
  }

  private class TestEnv(implicit p: Parameters) extends LazyModule {
    val visibility = TLEphemeralNode()
    val cache = LazyModule(new TestICache()(p.alterPartial {
      case TileVisibilityNodeKey => visibility
    }))
    val manager = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(
      TLSlaveParameters.v1(address = Seq(AddressSet(0, BigInt("ffffffff", 16))),
        supportsGet = TransferSizes(1, 64))), beatBytes = 8)))
    manager := visibility := cache.masterNode

    lazy val module = new TestEnvModule(this)
  }

  private class TestEnvModule(outer: TestEnv) extends LazyModuleImp(outer) {
      val dut = outer.cache.module
      val tl = outer.manager.in.head._1
      val io = IO(new Bundle {
        val lookup_valid = Input(Bool())
        val lookup_pc = Input(UInt(32.W))
        val lookup_fault = Input(UInt(2.W))
        val lookup_tlb_miss = Input(Bool())
        val lookup_prefetchable = Input(Bool())
        val lookup_ready = Output(Bool())
        val fetch_valid = Input(Bool())
        val fetch_pc = Input(UInt(32.W))
        val fetch_ready = Output(Bool())
        val response_valid = Output(Bool())
        val response_data = Output(UInt(128.W))
        val ifu_ready = Input(Bool())
        val flush = Input(Bool())
        val invalidate = Input(Bool())
        val acquire_ready = Input(Bool())
        val acquire_valid = Output(Bool())
        val acquire_address = Output(UInt(32.W))
        val acquire_source = Output(UInt(tl.a.bits.source.getWidth.W))
        val grant_valid = Input(Bool())
        val grant_source = Input(UInt(tl.d.bits.source.getWidth.W))
        val grant_data = Input(UInt(64.W))
        val grant_ready = Output(Bool())
        val miss_pending = Output(Bool())
        val refill_write = Output(Bool())
        val way_enq = Output(Bool())
        val way_mask = Output(UInt(cacheParams.nWays.W))
      })

      dut.io.prefetch_req.valid := io.lookup_valid
      dut.io.prefetch_req.bits.fetch_pc := io.lookup_pc
      dut.io.prefetch_req.bits.mask := 0.U
      io.lookup_ready := dut.io.prefetch_req.ready
      dut.io.req.valid := io.fetch_valid
      dut.io.req.bits := 0.U.asTypeOf(dut.io.req.bits)
      dut.io.req.bits.addr := io.fetch_pc
      io.fetch_ready := dut.io.req.ready
      io.response_valid := dut.io.resp.valid
      io.response_data := dut.io.resp.bits.data
      dut.io.ifu_s3_ready := io.ifu_ready
      dut.io.flush := io.flush
      dut.io.invalidate := io.invalidate
      dut.io.prefetch_tlb_req.ready := true.B
      dut.io.prefetch_tlb_resp := 0.U.asTypeOf(dut.io.prefetch_tlb_resp)
      dut.io.prefetch_tlb_resp.paddr := dut.io.prefetch_tlb_req.bits.vaddr
      dut.io.prefetch_tlb_resp.miss := io.lookup_tlb_miss
      dut.io.prefetch_tlb_resp.ae.inst := io.lookup_fault === 1.U
      dut.io.prefetch_tlb_resp.pf.inst := io.lookup_fault === 2.U
      dut.io.prefetch_tlb_resp.gf.inst := io.lookup_fault === 3.U
      dut.io.prefetch_tlb_resp.prefetchable := io.lookup_prefetchable
      dut.io.prefetch_tlb_resp.cacheable := io.lookup_prefetchable
      dut.io.prefetch_tlb_v := false.B
      dut.io.prefetch_tlb_prv := 3.U

      tl.a.ready := io.acquire_ready
      io.acquire_valid := tl.a.valid
      io.acquire_address := tl.a.bits.address
      io.acquire_source := tl.a.bits.source
      tl.d.valid := io.grant_valid
      tl.d.bits := 0.U.asTypeOf(tl.d.bits)
      tl.d.bits.opcode := TLMessages.AccessAckData
      tl.d.bits.size := 6.U
      tl.d.bits.source := io.grant_source
      tl.d.bits.data := io.grant_data
      io.grant_ready := tl.d.ready
      tl.b.valid := false.B
      tl.b.bits := DontCare
      tl.c.ready := true.B
      tl.e.ready := true.B

      io.miss_pending := dut.probe.miss_pending
      io.refill_write := dut.probe.refill_write
      io.way_enq := dut.probe.way_enq
      io.way_mask := dut.probe.way_mask
  }

  private def zeroInputs(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(zeroInputs)
    case v: Vec[_] => v.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf.poke(BigInt(0))
    case _ => ()
  }

  def main(args: Array[String]): Unit = simulate(LazyModule(new TestEnv()(parameters)).module) { d =>
    val acquisitions = scala.collection.mutable.ArrayBuffer.empty[(BigInt, BigInt)]
    def tick(): Unit = {
      if (d.io.acquire_valid.peek().litToBoolean && d.io.acquire_ready.peek().litToBoolean) {
        acquisitions += ((d.io.acquire_address.peek().litValue, d.io.acquire_source.peek().litValue))
      }
      d.clock.step()
    }
    def waitUntil(condition: => Boolean, message: String, limit: Int = 40): Unit = {
      var cycles = 0
      while (!condition && cycles < limit) { tick(); cycles += 1 }
      assert(condition, message)
    }
    def lookup(pc: BigInt): Unit = {
      d.io.lookup_pc.poke(pc); d.io.lookup_valid.poke(true)
      waitUntil(d.io.lookup_ready.peek().litToBoolean, "prefetch blocked while a miss is pending")
      tick(); d.io.lookup_valid.poke(false)
    }
    def fetch(pc: BigInt): Unit = {
      d.io.fetch_pc.poke(pc); d.io.fetch_valid.poke(true)
      waitUntil(d.io.fetch_ready.peek().litToBoolean, "fetch did not consume its WayLookup entry")
      tick(); d.io.fetch_valid.poke(false)
    }
    def refill(pc: BigInt, word: BigInt, concurrentLookup: Boolean = false): Unit = {
      val source = acquisitions.reverseIterator.find(_._1 == pc).get._2
      d.io.grant_source.poke(source); d.io.grant_data.poke(word)
      d.io.grant_valid.poke(true)
      for (beat <- 0 until 8) {
        d.io.grant_ready.expect(true.B)
        d.io.refill_write.expect(false.B)
        if (beat == 7 && concurrentLookup) {
          d.io.lookup_pc.poke(pc); d.io.lookup_valid.poke(true)
          d.io.lookup_ready.expect(true.B)
        }
        tick()
      }
      d.io.grant_valid.poke(false); d.io.lookup_valid.poke(false)
      d.io.refill_write.expect(true.B)
      d.io.lookup_ready.expect(false.B)
      d.io.fetch_ready.expect(false.B)
    }
    def expectResponse(word: BigInt): Unit = {
      waitUntil(d.io.response_valid.peek().litToBoolean, "cached fetch did not return data")
      d.io.response_data.expect((word << 64) | word)
    }

    zeroInputs(d.io)
    d.io.lookup_prefetchable.poke(true)
    d.io.acquire_ready.poke(true); d.io.ifu_ready.poke(true)
    d.reset.poke(true); d.clock.step(2); d.reset.poke(false); tick()
    val a = BigInt("80000000", 16)
    val b = a + 64
    val c = a + 128
    val wordA = BigInt("0123456789abcdef", 16)
    val wordB = BigInt("1122334455667788", 16)
    val wordC = BigInt("8877665544332211", 16)

    lookup(a)
    waitUntil(acquisitions.size == 1, "first prefetch did not reach L2")
    for (_ <- 0 until 20) {
      d.io.miss_pending.expect(true.B)
      d.io.refill_write.expect(false.B)
      d.io.lookup_ready.expect(true.B)
      tick()
    }
    // Demand A waits for data, while independent B/C lookups continue ahead.
    fetch(a)
    d.io.response_valid.expect(false.B)
    lookup(b); lookup(c)
    waitUntil(acquisitions.size >= 3, "independent misses were serialized behind A")
    assert(acquisitions.map(_._1).toSeq == Seq(a, b, c), "duplicate or reordered L2 requests")

    // A metadata read accepted on the last D beat returns during the SRAM
    // write. Its WayLookup entry must use the existing refill bypass.
    d.io.ifu_ready.poke(false)
    refill(a, wordA, concurrentLookup = true)
    d.io.way_enq.expect(true.B)
    assert(d.io.way_mask.peek().litValue != 0, "same-cycle refill bypass lost its cache hit")
    expectResponse(wordA)
    d.io.fetch_pc.poke(b); d.io.fetch_valid.poke(true)
    d.io.fetch_ready.expect(false.B)
    tick()
    d.io.refill_write.expect(false.B)
    d.io.lookup_ready.expect(true.B)
    for (_ <- 0 until 3) { expectResponse(wordA); tick() }
    d.io.fetch_valid.poke(false); d.io.ifu_ready.poke(true)
    expectResponse(wordA); tick()

    // Refills update B/C entries already waiting in WayLookup.
    refill(b, wordB); tick()
    refill(c, wordC); tick()
    d.io.miss_pending.expect(false.B)
    fetch(b); expectResponse(wordB); tick()
    fetch(c); expectResponse(wordC); tick()
    fetch(a); expectResponse(wordA); tick()
    assert(acquisitions.size == 3, "cache hits unexpectedly allocated another miss")

    d.io.lookup_valid.poke(true); d.io.fetch_valid.poke(true)
    d.io.flush.poke(true); d.io.invalidate.poke(true)
    d.io.lookup_ready.expect(false.B); d.io.fetch_ready.expect(false.B)
    tick()
    d.io.lookup_valid.poke(false); d.io.fetch_valid.poke(false)
    d.io.flush.poke(false); d.io.invalidate.poke(false)
    lookup(a)
    waitUntil(acquisitions.size == 4, "invalidate retained the old cache hit")
    assert(acquisitions.last._1 == a)
    refill(a, wordA); tick()
    d.io.miss_pending.expect(false.B)

    // A speculative fetch fault must keep its place in WayLookup without
    // issuing a memory request. Demand consumes it so IFU can raise the fault.
    d.io.flush.poke(true); tick(); d.io.flush.poke(false)
    for (fault <- 1 to 3; replay <- Seq(false, true)) {
      val faultPc = a - 640
      d.io.lookup_fault.poke(fault)
      d.io.lookup_tlb_miss.poke(replay)
      lookup(faultPc)
      if (replay) {
        for (_ <- 0 until 3) { tick() }
        d.io.lookup_tlb_miss.poke(false)
      }
      for (_ <- 0 until 8) { tick() }
      assert(acquisitions.size == 4, "faulting speculative lookup issued a TileLink Get")
      fetch(faultPc)
      waitUntil(d.io.response_valid.peek().litToBoolean, "faulting demand fetch did not complete")
      tick()
      assert(acquisitions.size == 4, "faulting demand fetch issued a TileLink Get")
    }
    d.io.lookup_fault.poke(0)

    d.io.lookup_prefetchable.poke(false)
    val demandOnly = a + 192
    lookup(demandOnly)
    for (_ <- 0 until 8) { tick() }
    assert(acquisitions.size == 4, "non-prefetchable lookup issued a speculative Get")
    fetch(demandOnly)
    waitUntil(acquisitions.size == 5, "non-prefetchable demand request was dropped")
    assert(acquisitions.last._2 < 4, "non-prefetchable request used a prefetch MSHR")
    refill(demandOnly, wordA)
    expectResponse(wordA); tick()
    println("PASS: concurrent misses, demand overlap, write-cycle gating, refill bypass, queued-entry updates, retained data, invalidate, fetch faults, TLB replay and non-prefetchable demand")
  }
}
