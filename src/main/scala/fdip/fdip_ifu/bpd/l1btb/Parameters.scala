package boom.fdip.ifu.bpd.l1btb

/** Configuration owned by the FDIP level-1 BTB predictor. */
case class BoomL1BTBParams(
  nSets: Int = 128,
  nWays: Int = 4,
  offsetSz: Int = 20
)

trait HasL1BtbParameters extends boom.fdip.ifu.bpd.HasBpdParameters {
  def l1BtbParams: BoomL1BTBParams = l1btbParameters
  def L1BtbWays: Int = l1BtbParams.nWays
  def L1BtbSets: Int = l1BtbParams.nSets
  def L1BtbOffsetWidth: Int = l1BtbParams.offsetSz
  def L1BtbTagWidth: Int = vaddrBitsExtended - log2Ceil(L1BtbSets) - log2Ceil(fetchBytes) - 1
}
