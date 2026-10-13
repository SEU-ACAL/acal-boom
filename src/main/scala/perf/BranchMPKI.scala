package boom.perf

import chisel3._

/** Shared ISA/RAS-hint classification for FDIP and coupled BOOMv3 counters. */
object BranchMPKI {
  val kinds = Seq("cond", "call", "return", "directuncond", "indirectuncond")
  val causes = Seq("direction_taken", "direction_not_taken", "target_or_position")

  def classify(
    isCond: Bool, isJal: Bool, isJalr: Bool, rd: UInt, rs1: UInt
  ): Seq[(String, Bool)] = {
    val linkDest = rd === 1.U || rd === 5.U
    val linkSrc = rs1 === 1.U || rs1 === 5.U
    // PopAndPush is a call, and must not also contribute to return MPKI.
    val call = !isCond && (isJal || isJalr) && linkDest
    val ret = !isCond && !call && isJalr && linkSrc && rd =/= rs1
    val direct = !isCond && !call && !ret && isJal
    val indirect = !isCond && !call && !ret && !direct && isJalr
    Seq("cond" -> isCond, "call" -> call, "return" -> ret,
      "directuncond" -> direct, "indirectuncond" -> indirect)
  }

  /** Gate the returned events with the same b2.mispredict as the legacy total. */
  def mispredictBreakdown(
    isCond: Bool, isJal: Bool, isJalr: Bool, rd: UInt, rs1: UInt,
    taken: Bool, predictedTaken: Bool
  ): Seq[(String, Bool)] = {
    val classes = classify(isCond, isJal, isJalr, rd, rs1)
    val withOther = classes :+ ("other" -> !classes.map(_._2).reduce(_ || _))
    // Equal direction does not distinguish target mismatch from CFI position.
    // These are backend prediction fields, not comparisons between BPU stages.
    val causeMasks = Seq(
      "direction_taken" -> (taken && !predictedTaken),
      "direction_not_taken" -> (!taken && predictedTaken),
      "target_or_position" -> (taken === predictedTaken))
    val matrix = for ((kind, selected) <- withOther; (cause, mask) <- causeMasks)
      yield s"${kind}_${cause}" -> (selected && mask)
    val call = classes(1)._2
    withOther ++ causeMasks ++ matrix ++ Seq(
      "call_direct" -> (call && isJal),
      "call_indirect" -> (call && !isJal && isJalr),
      "call_popandpush" -> (call && isJalr &&
        (rs1 === 1.U || rs1 === 5.U) && rd =/= rs1))
  }
}
