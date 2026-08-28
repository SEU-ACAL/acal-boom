package boom.fdip.ifu.bpd.l1btb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import boom.fdip.common._
import boom.fdip.ifu._
import boom.fdip.ifu.bpd._

trait L1BTBHelper extends HasL1BtbParameters with TargetFixHelper{

  def getTargetUpper(pc: UInt): UInt = {
    pc(vaddrBitsExtended - 1, L1BtbOffsetWidth + instOffsetBits)
  }
}
