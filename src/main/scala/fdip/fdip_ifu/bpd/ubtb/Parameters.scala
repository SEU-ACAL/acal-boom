package boom.fdip.ifu.bpd.ubtb

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
