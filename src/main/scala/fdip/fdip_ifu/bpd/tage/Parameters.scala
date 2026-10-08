package boom.fdip.ifu.bpd.tage

/** Configuration owned by the FDIP TAGE predictor. */
case class BoomTageParams(
  tableInfo: Seq[Tuple3[Int, Int, Int]] = Seq((1024, 4, 10),
                                              (1024, 8, 10),
                                              (1024, 13, 10),
                                              (1024, 16, 10),
                                              (1024, 32, 10),
                                              (1024, 119, 10)),
  uBitPeriod: Int = 2048,
  tagBits: Int = 10,
  nWays: Int = 2,
  writeBypassEntries: Int = 4
) {
  require(writeBypassEntries > 0, "TAGE write bypass must retain at least one write event")
}

trait HasTageParameters extends boom.fdip.ifu.bpd.HasBpdParameters {
  def tageParams: BoomTageParams = tageParameters
  def TageTables: Int = tageParams.tableInfo.size
  def TageTagWidth: Int = tageParams.tagBits
  def TageUsefulPeriod: Int = tageParams.uBitPeriod
  def TageNways: Int = tageParams.nWays
}
