package boom.fdip.ifu.bpd.ras

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import boom.fdip.common._
import boom.fdip.util._
import boom.fdip.ifu._
import boom.fdip.ifu.bpd.l1btb.BoomL1BTBParams
import boom.fdip.ifu.bpd.ubtb.BoomFAMicroBTBParams
import boom.fdip.ifu.bpd.tage.BoomTageParams
/** Configuration owned by the return-address stack predictor. */

case class BoomRasParams(
  CommitStackSize:   Int = 16, // Size of the RAS stack
  SpecQueueSize:     Int = 32, // Size of the RAS speculative queue
  StackCounterWidth: Int = 3
)

trait HasRasParameters extends boom.fdip.ifu.bpd.HasBpdParameters {
  def rasParams: BoomRasParams = rasParameters
  def RasCommitStackSize: Int = rasParams.CommitStackSize
  def RasSpecQueueSize: Int = rasParams.SpecQueueSize
  def RasStackCounterWidth: Int = rasParams.StackCounterWidth
}
