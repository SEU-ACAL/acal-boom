package boom.fdip.ifu.bpd.ras

import chisel3._
import chisel3.util._
import boom.fdip.common._
import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._

class RasState(implicit p: Parameters) extends BoomBundle()(p) with HasRasParameters{
    val idx = UInt(log2Ceil(nRasEntries).W)
    val top = UInt(vaddrBitsExtended.W)
}