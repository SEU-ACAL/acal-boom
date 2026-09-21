package boom.fdip.ifu.bpd

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import boom.fdip.common._
import boom.fdip.util._
import boom.fdip.ifu._
import boom.fdip.ifu.bpd.l1btb.BoomL1BTBParams
import boom.fdip.ifu.bpd.ubtb.BoomFAMicroBTBParams
import boom.fdip.ifu.bpd.tage.BoomTageParams
import boom.fdip.ifu.bpd.tage.TagePredictMeta
import boom.fdip.ifu.bpd.l1btb.BTBPredictMeta
import boom.fdip.ifu.bpd.ras.RasState
class PhrPtr(entries: Int)(implicit p: Parameters)
  extends CircularQueuePtr[PhrPtr](entries) {
  override protected def make(f: Bool, v: UInt): PhrPtr = {
    val ptr = Wire(new PhrPtr(entries))
    ptr.flag := f
    ptr.value := v
    ptr
  }
}
//used for PHR update
class PhrPredictMeta(implicit p: Parameters) extends PHRBundle()(p) with HasBpdParameters {
  val ptr = new PhrPtr(PHRWidth)
  val taken = Bool()
  val cfiPc = UInt(vaddrBitsExtended.W)
  val target = UInt(vaddrBitsExtended.W)
}
