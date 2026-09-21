package boom.fdip.ifu.bpd.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import boom.fdip.common.BoomBundle

/**
  * Circular pointer used by history queues (for example PHR) and FTQ-like
  * structures.
  *
  * This is the BOOM/FDIP equivalent of XiangShan's CircularQueuePtr.  The
  * pointer contains both an index and a wrap bit.  Keeping the wrap bit is
  * important for recovery: two pointers with the same index can refer to
  * different logical positions after a full turn around the ring.
  *
  * `inc` and `dec` intentionally take Scala constants.  PHR updates are a
  * fixed number of bits per cycle, so this gives a small synthesizable
  * implementation and avoids introducing a divider/modulo datapath.
  */
abstract class CircularQueuePtr[T <: CircularQueuePtr[T]](
  val entries: Int
)(implicit p: Parameters) extends BoomBundle {
  require(entries >= 2, "a circular pointer needs at least two entries")

  val flag  = Bool()
  val value = UInt(log2Ceil(entries).W)

  /** Construct a typed hardware pointer from the supplied fields. */
  protected def make(f: Bool, v: UInt): T

  /** Full pointer equality, including the wrap bit. */
  def ===(that: T): Bool = flag === that.flag && value === that.value

  def =/=(that: T): Bool = !(this === that)

  /** Index equality, intentionally ignoring the wrap bit. */
  def sameIndex(that: T): Bool = value === that.value

  /**
    * True when this pointer is ahead of `that` by less than one ring.
    * This is the normal queue-pointer ordering and assumes that pointers are
    * never compared after more than one complete ring of outstanding entries.
    */
  def isAfter(that: T): Bool = Mux(
    flag === that.flag,
    value > that.value,
    value <= that.value
  )

  def isBefore(that: T): Bool = Mux(
    flag === that.flag,
    value < that.value,
    value >= that.value
  )

  /** Advance by a compile-time constant number of entries. */
  def inc(amount: Int = 1): T = {
    require(amount >= 0, "increment must be non-negative")
    var result: T = make(flag, value)
    for (_ <- 0 until amount) {
      val wrapped = result.value === (entries - 1).U
      result = make(result.flag ^ wrapped, Mux(wrapped, 0.U, result.value + 1.U))
    }
    result
  }

  /** Retreat by a compile-time constant number of entries. */
  def dec(amount: Int = 1): T = {
    require(amount >= 0, "decrement must be non-negative")
    var result: T = make(flag, value)
    for (_ <- 0 until amount) {
      val wrapped = result.value === 0.U
      result = make(result.flag ^ wrapped, Mux(wrapped, (entries - 1).U, result.value - 1.U))
    }
    result
  }

  def next: T = inc(1)
  def prev: T = dec(1)
}
