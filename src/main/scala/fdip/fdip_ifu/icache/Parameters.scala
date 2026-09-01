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
  def icacheParams: ICacheParams = ICacheParams
  def nSets: Int = icacheParams.nSets
  def tagBits: Int = icacheParams.blockBytes*8
  def nSetsBits: Int = log2Ceil(nSets)
  def nWaysBits: Int = log2Ceil(nWays)
  def nWays: Int = icacheParams.nWays
  def blockBytes: Int = icacheParams.blockBytes
  def fetchBytes: Int = icacheParams.fetchBytes
  def WayLookupEntrySz: Int = 16
}
