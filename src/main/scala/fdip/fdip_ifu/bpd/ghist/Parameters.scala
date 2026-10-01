package boom.fdip.ifu.bpd

import boom.fdip.ifu.bpd.l1btb.BoomL1BTBParams
import boom.fdip.ifu.bpd.ras.BoomRasParams
import boom.fdip.ifu.bpd.tage.BoomTageParams
import boom.fdip.ifu.bpd.ubtb.BoomFAMicroBTBParams
import chisel3.util.log2Ceil


/** Configuration owned by the FDIP PHR predictor. */
case class BoomPHRParams(
  Shamt: Int = 3,
  HashWidth: Int = 10,
  HashHighWidth: Int = 7
)

trait HasPhrParameters extends boom.fdip.ifu.bpd.HasBpdParameters
    with boom.fdip.common.HasBoomCoreParameters {
  def phrParameters: BoomPHRParams = bpdParameters.phr
  def phrParams: BoomPHRParams = phrParameters
  def PHRShamt: Int = phrParams.Shamt
  def PHRHashWidth: Int = phrParams.HashWidth
  def PHRHashHighWidth: Int = phrParams.HashHighWidth
  //protect PHR
  def PHRWidth:Int = globalHistoryLength + PHRShamt * ftqSz
  def PHRPtrWidth:Int = log2Ceil(PHRWidth)
}
