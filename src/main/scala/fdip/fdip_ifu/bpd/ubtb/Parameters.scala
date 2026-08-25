package boom.fdip.ifu.bpd.ubtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import boom.fdip.common._
import boom.fdip.util._
import boom.fdip.ifu._
import boom.fdip.ifu.bpd.l1btb.BoomL1BTBParams
import boom.fdip.ifu.bpd.ubtb.BoomFAMicroBTBParams
import boom.fdip.ifu.bpd.tage.BoomTageParams
/** Configuration owned by the fully-associative micro-BTB predictor. */
case class BoomFAMicroBTBParams(
  nEntries: Int = 16,
  offsetSz: Int = 13
)

trait HasUbtbParameters extends boom.fdip.ifu.bpd.HasBpdParameters {
  def ubtbParams: BoomFAMicroBTBParams = ubtbParameters
  def UbtbEntries: Int = ubtbParams.nEntries
  def UbtbOffsetWidth: Int = ubtbParams.offsetSz
  def UbtbTagWidth: Int = vaddrBitsExtended - log2Ceil(fetchBytes) - 1
}
