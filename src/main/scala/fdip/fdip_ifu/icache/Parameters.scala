package boom.fdip.ifu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import boom.fdip.common._
import boom.fdip.util._
import boom.fdip.ifu._

/** Configuration owned by the fully-associative micro-BTB predictor. */
case class BoomFAMicroBTBParams(
  nEntries: Int = 16,
  offsetSz: Int = 13
)

trait HasICacheParameters extends HasBoomFrontendParameters with HasBoomCoreParameters {
  def blockBytes: Int = cacheParams.blockBytes
  def nSetsBits: Int = log2Ceil(nSets)
  def nWaysBits: Int = log2Ceil(nWays)
  def fetchBankNum: Int = fetchBytes / (blockBytes / numBanks)
  def WayLookupEntrySz: Int = 16
  def numBanks: Int = 8
  def bankBits = (blockBytes/numBanks)*8
  val bankOffBits = log2Up(numBanks)
}
