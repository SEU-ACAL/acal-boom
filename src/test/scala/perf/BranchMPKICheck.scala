package boom.perf

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import chisel3.util._

/** Run with: sbt 'boom/Test/runMain boom.perf.BranchMPKICheck'. */
object BranchMPKICheck {
  private class Probe extends Module {
    val io = IO(new Bundle {
      val cfi = Input(UInt(2.W)) // 0=non-CFI, 1=conditional, 2=JAL, 3=JALR
      val rd = Input(UInt(5.W))
      val rs1 = Input(UInt(5.W))
      val taken = Input(Bool())
      val predictedTaken = Input(Bool())
      val mispredict = Input(Bool())
      val classMask = Output(UInt(6.W))
      val counts = Output(Vec(30, UInt(16.W)))
    })
    val events = BranchMPKI.mispredictBreakdown(
      io.cfi === 1.U, io.cfi === 2.U, io.cfi === 3.U, io.rd, io.rs1,
      io.taken, io.predictedTaken)
    require(events.size == io.counts.size)
    val names = events.map(_._1)
    val counts = RegInit(VecInit(Seq.fill(events.size)(0.U(16.W))))
    for (((_, event), index) <- events.zipWithIndex) {
      when(io.mispredict && event) { counts(index) := counts(index) + 1.U }
    }
    io.classMask := VecInit(events.take(6).map(_._2)).asUInt
    io.counts := counts
  }

  def main(args: Array[String]): Unit = simulate(new Probe) { d =>
    // Expected kinds are ISA examples, including x5 and coroutine RAS hints.
    val cases = Seq(
      (0, 1, 5, "other", false),
      (1, 1, 5, "cond", false),
      (2, 0, 0, "directuncond", false),
      (2, 1, 0, "call", false),
      (2, 5, 0, "call", false),
      (2, 10, 0, "directuncond", false),
      (3, 0, 1, "return", false),
      (3, 0, 5, "return", false),
      (3, 10, 1, "return", false),
      (3, 1, 10, "call", false),
      (3, 5, 10, "call", false),
      (3, 1, 1, "call", false),
      (3, 5, 5, "call", false),
      (3, 1, 5, "call", true),
      (3, 5, 1, "call", true),
      (3, 0, 10, "indirectuncond", false),
      (3, 10, 11, "indirectuncond", false))
    val expected = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
    d.io.cfi.poke(0); d.io.rd.poke(0); d.io.rs1.poke(0)
    d.io.taken.poke(false); d.io.predictedTaken.poke(false); d.io.mispredict.poke(false)
    d.reset.poke(true); d.clock.step(2); d.reset.poke(false)
    for ((cfi, rd, rs1, kind, popAndPush) <- cases;
         (taken, predicted, cause) <- Seq(
           (true, false, "direction_taken"), (false, true, "direction_not_taken"),
           (true, true, "target_or_position"), (false, false, "target_or_position"))) {
      d.io.cfi.poke(cfi); d.io.rd.poke(rd); d.io.rs1.poke(rs1)
      d.io.taken.poke(taken); d.io.predictedTaken.poke(predicted)
      d.io.classMask.expect(BigInt(1) << (BranchMPKI.kinds :+ "other").indexOf(kind))
      // A classification on an inactive/stale b2 packet must not increment.
      d.io.mispredict.poke(false); d.clock.step()
      d.io.mispredict.poke(true); d.clock.step()
      for (name <- Seq(kind, cause, s"${kind}_${cause}")) expected(name) += 1
      if (kind == "call") expected(if (cfi == 2) "call_direct" else "call_indirect") += 1
      if (popAndPush) expected("call_popandpush") += 1
    }
    d.io.mispredict.poke(false); d.clock.step()
    for ((name, index) <- d.names.zipWithIndex) d.io.counts(index).expect(expected(name))
    println("PASS: five exclusive CFI kinds, x1/x5, PopAndPush, cause partitions and event gating")
  }
}
