package boom.fdip.ifu.bpd.l1btb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

/**
  * Per-set True-LRU replacement state for the FDIP L1 BTB.
  *
  * The state encoding follows XiangShan's LruStateGen: one bit is stored for
  * every pair of ways. For a pair (low, high), the bit is one when `high` is
  * more recent than `low`. The triangular matrix is packed row by row.
  *
  * Victim selection adds a candidate mask to the same pairwise ordering. It
  * therefore preserves LRU ordering after masking and does not fall back to a
  * fixed way-0 priority. The t0/t1 state bypass follows XiangShan's
  * MainBtbReplacer timing protocol.
  */
class L1BtbReplacer(implicit p: Parameters) extends L1BtbModule()(p) {
  private val replacerWayBits = log2Ceil(L1BtbWays max 2)
  private val setBits = log2Ceil(L1BtbSets max 2)
  private val stateWidth = L1BtbWays * (L1BtbWays - 1) / 2
  private val initialState = 0.U(stateWidth.W)

  val io = IO(new Bundle {
    val t0SetIdx = Input(UInt(setBits.W))
    val t0Fire = Input(Bool())
    val t1SetIdx = Input(UInt(setBits.W))
    val t1TouchValid = Input(Bool())
    val t1TouchWay = Input(UInt(replacerWayBits.W))
    val t1CandidateMask = Input(UInt(L1BtbWays.W))
    val t1Victim = Output(UInt(replacerWayBits.W))
    val t1State = Output(UInt(stateWidth.W))
    val t1NextState = Output(UInt(stateWidth.W))
  })

  require(L1BtbWays >= 2, "L1BtbReplacer requires at least two ways")

  private val stateBank = RegInit(VecInit(Seq.fill(L1BtbSets)(initialState)))

  /** Reconstruct XiangShan's lower-triangular pairwise relation matrix. */
  private def extractMRUVec(state: UInt): Vec[UInt] = {
    val moreRecentVec = Wire(Vec(L1BtbWays - 1, UInt(L1BtbWays.W)))
    var lsb = 0
    for (i <- 0 until L1BtbWays - 1) {
      moreRecentVec(i) := Cat(
        state(lsb + L1BtbWays - i - 2, lsb),
        0.U((i + 1).W)
      )
      lsb += L1BtbWays - i - 1
    }
    moreRecentVec
  }

  /** Pack matrix rows in the same order as XiangShan's LruStateGen. */
  private def packState(rows: Vec[UInt]): UInt = {
    Cat(rows.zipWithIndex.reverse.map { case (row, i) => row(L1BtbWays - 1, i + 1) })
  }

  private def nextState(state: UInt, touchValid: Bool, touchWay: UInt): UInt = {
    val moreRecentVec = extractMRUVec(state)
    val wayDec = UIntToOH(touchWay, L1BtbWays)
    val nextRows = Wire(Vec(L1BtbWays - 1, UInt(L1BtbWays.W)))

    nextRows.zipWithIndex.foreach { case (row, i) =>
      row := Mux(i.U === touchWay, 0.U(L1BtbWays.W), moreRecentVec(i) | wayDec)
    }

    Mux(touchValid, packState(nextRows), state)
  }

  /** Whether way `a` is newer than way `b` in the triangular matrix. */
  private def isMoreRecent(moreRecentVec: Vec[UInt], a: Int, b: Int): Bool = {
    require(a != b)
    if (a > b) moreRecentVec(b)(a) else !moreRecentVec(a)(b)
  }

  /** Select the oldest way among the supplied candidates. */
  private def oldestCandidate(state: UInt, candidateMask: UInt): UInt = {
    val moreRecentVec = extractMRUVec(state)
    val allWaysMask = ((BigInt(1) << L1BtbWays) - 1).U(L1BtbWays.W)
    val effectiveMask = Mux(candidateMask.orR, candidateMask, allWaysMask)
    val victimOH = Wire(Vec(L1BtbWays, Bool()))

    for (victim <- 0 until L1BtbWays) {
      val olderThanAllCandidates = (0 until L1BtbWays)
        .filter(_ != victim)
        .map { other =>
          !effectiveMask(other) || isMoreRecent(moreRecentVec, other, victim)
        }
        .reduce(_ && _)
      victimOH(victim) := effectiveMask(victim) && olderThanAllCandidates
    }

    OHToUInt(victimOH.asUInt)
  }

  private val t1StateFromT0 = RegInit(initialState)
  private val t1NextState = nextState(t1StateFromT0, io.t1TouchValid, io.t1TouchWay)
  private val trainState = Mux(
    io.t1TouchValid && io.t1SetIdx === io.t0SetIdx,
    t1NextState,
    stateBank(io.t0SetIdx)
  )

  when (io.t0Fire) {
    t1StateFromT0 := trainState
  }

  io.t1State := t1StateFromT0
  io.t1NextState := t1NextState
  io.t1Victim := oldestCandidate(t1StateFromT0, io.t1CandidateMask)

  when (io.t1TouchValid) {
    stateBank(io.t1SetIdx) := t1NextState
  }
}
