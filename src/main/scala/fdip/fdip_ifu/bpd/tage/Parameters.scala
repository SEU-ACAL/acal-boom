package boom.fdip.ifu.bpd.tage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
/** Configuration owned by the FDIP TAGE predictor. */
case class BoomTageParams(
  tableInfo: Seq[Tuple3[Int, Int, Int]] = Seq((512, 4, 10),
                                              (512, 8, 10),
                                              (512, 13, 10),
                                              (512, 16, 10),
                                              (512, 32, 10),
                                              (1024, 119, 10)),
  uBitPeriod: Int = 2048,
  tagBits: Int = 10,
  nWays: Int = 2,
  useAltOnNaWidth: Int = 5,
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
  def NumUseAltOnNa:       Int = 2 * TageTables // each table corresponds to two counters
  def UseAltOnNaIdxWidth:  Int = log2Ceil(NumUseAltOnNa)
  def UseAltOnNaWidth:     Int = tageParams.useAltOnNaWidth
}
