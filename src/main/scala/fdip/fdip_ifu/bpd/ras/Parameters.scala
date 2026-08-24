package boom.fdip.ifu.bpd.ras

/** Configuration owned by the return-address stack predictor. */
case class BoomRasParams(entries: Int = 32)

trait HasRasParameters extends boom.fdip.ifu.bpd.HasBpdParameters {
  def rasParams: BoomRasParams = rasParameters
  def RasEntries: Int = rasParams.entries max 2
}
