package boom.fdip.ifu.bpd.ubtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import boom.fdip.common._
import boom.fdip.ifu._
import boom.fdip.ifu.bpd._

trait UBTBHelper extends HasUbtbParameters with TargetFixHelper{

  def getTargetUpper(pc: UInt): UInt = {
    pc(vaddrBitsExtended - 1, UbtbOffsetWidth + instOffsetBits)
  }
}
