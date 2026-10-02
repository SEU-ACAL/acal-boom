package boom.fdip.ifu.bpd.tage

/** Configuration owned by the FDIP TAGE predictor. */
case class BoomTageParams(
  tableInfo: Seq[Tuple3[Int, Int, Int]] = Seq((128, 2, 10),
                                              (128, 4, 10),
                                              (128, 8, 10),
                                              (128, 16, 10),
                                              (128, 32, 10),
                                              (128, 64, 10)),
  uBitPeriod: Int = 2048,
  tagBits: Int = 10,
  writeBypassEntries: Int = 4
) {
  require(writeBypassEntries > 0, "TAGE write bypass must retain at least one write event")
}

trait HasTageParameters extends boom.fdip.ifu.bpd.HasBpdParameters {
  def tageParams: BoomTageParams = tageParameters
  def TageTables: Int = tageParams.tableInfo.size
  def TageTagWidth: Int = tageParams.tagBits
  def TageUsefulPeriod: Int = tageParams.uBitPeriod
}
