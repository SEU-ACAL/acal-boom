package boom.fdip.ifu.bpd

import boom.fdip.ifu.bpd.l1btb.BoomL1BTBParams
import boom.fdip.ifu.bpd.ras.BoomRasParams
import boom.fdip.ifu.bpd.tage.BoomTageParams
import boom.fdip.ifu.bpd.ubtb.BoomFAMicroBTBParams
import boom.fdip.ifu.bpd.BoomPHRParams

/** Complete predictor configuration owned by the frontend BPU. */
case class BoomBpdParams(
  ubtb:  BoomFAMicroBTBParams = BoomFAMicroBTBParams(),
  l1btb: BoomL1BTBParams      = BoomL1BTBParams(),
  tage:  BoomTageParams        = BoomTageParams(),
  ras:   BoomRasParams         = BoomRasParams(),
  phr:   BoomPHRParams         = BoomPHRParams()
)

trait HasBpdParameters extends boom.fdip.ifu.HasBoomFrontendParameters {
  def bpdParameters: BoomBpdParams =
    tileParams.core.asInstanceOf[boom.fdip.common.BoomCoreParams].bpd

  def ubtbParameters: BoomFAMicroBTBParams = bpdParameters.ubtb
  def l1btbParameters: BoomL1BTBParams      = bpdParameters.l1btb
  def tageParameters: BoomTageParams        = bpdParameters.tage
  def rasParameters: BoomRasParams         = bpdParameters.ras
}
