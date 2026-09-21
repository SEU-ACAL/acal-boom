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
//used for PHR s1 update
class PhrMeta(implicit p: Parameters) extends PHRBundle()(p) with HasBpdParameters {
  val ptr = new PhrPtr(PHRWidth)
  val phrLowBits = UInt(PHRHashHighWidth.W)

}
class PhrS1Train(implicit p: Parameters) extends PHRBundle()(p) with HasBpdParameters {
  val valid = Bool()
  val taken = Bool()
  val cfiPc = UInt(vaddrBitsExtended.W)
  val target = UInt(vaddrBitsExtended.W)
}
class PhrUpdate(implicit p: Parameters) extends PHRBundle()(p) with HasBpdParameters {

  val valid = Bool()
  val cifPc = UInt(vaddrBitsExtended.W)
  val phrMeta = new PhrMeta
  val prediction = new Prediction

}

/** Result of restoring a PHR snapshot and optionally applying a taken CFI. */
class PhrUpdateResult(implicit p: Parameters) extends PHRBundle()(p) with HasBpdParameters {
  val ptr        = new PhrPtr(PHRWidth)
  val phrLowBits = UInt(PHRHashHighWidth.W)
}

class PhrTrain(implicit p: Parameters) extends PHRBundle()(p) with HasBpdParameters {

  val s2 = new PhrUpdate
  val redirect = Valid(new BpuRedirect)

}
