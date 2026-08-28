// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package boom.fdip.ifu.bpd

import chisel3._
import chisel3.util._

/**
  * Encodes how a partial branch target relates to the high bits of its
  * corresponding start PC.
  */
class TargetCarry extends Bundle {
  val value = UInt(TargetCarry.Width.W)

  def isFit: Bool       = value === TargetCarry.FitValue
  def isOverflow: Bool  = value === TargetCarry.OverflowValue
  def isUnderflow: Bool = value === TargetCarry.UnderflowValue
}

object TargetCarry {
  private val Width = 2
  private val FitValue = 0.U(Width.W)
  private val OverflowValue = 1.U(Width.W)
  private val UnderflowValue = 2.U(Width.W)

  private def apply(value: UInt): TargetCarry = {
    val carry = Wire(new TargetCarry)
    carry.value := value
    carry
  }

  def Fit: TargetCarry       = apply(FitValue)
  def Overflow: TargetCarry  = apply(OverflowValue)
  def Underflow: TargetCarry = apply(UnderflowValue)
}

trait TargetFixHelper extends HasBpdParameters {
  // abstract getTargetUpper function, to be implemented by sub-predictors.
  // basically, it should be `vAddr(VAddrBits - 1, TargetLowerBitsWidth + instOffsetBits)`,
  // where TargetLowerBitsWidth differs for different predictors.
  def getTargetUpper(pc: UInt): UInt

  def getTargetCarry(startPc: UInt, fullTarget: UInt): TargetCarry = {
    val startPcUpper = getTargetUpper(startPc)
    val targetUpper  = getTargetUpper(fullTarget)
    MuxCase(
      TargetCarry.Fit,
      Seq(
        (targetUpper > startPcUpper) -> TargetCarry.Overflow,
        (targetUpper < startPcUpper) -> TargetCarry.Underflow
      )
    )
  }

  def fixTargetUpper(
      startPcUpper: UInt,
      targetCarry:  TargetCarry,
      // pre-computed startPcUpper+-1 for timing, default is not needed
      startPcUpperPlusOne:  Option[UInt] = None,
      startPcUpperMinusOne: Option[UInt] = None
  ): UInt =
    MuxCase(
      startPcUpper,
      Seq(
        targetCarry.isOverflow  -> startPcUpperPlusOne.getOrElse(startPcUpper +% 1.U),
        targetCarry.isUnderflow -> startPcUpperMinusOne.getOrElse(startPcUpper -% 1.U)
      )
    )

  // ubtb and abtb can select whether to use the targetCarry or not, so Option[TargetCarry]
  def getFullTarget(
      startPc:     UInt,
      target:      UInt,
      targetCarry: Option[TargetCarry] = None,
      // pre-computed startPcUpper+-1 for timing, default is not needed
      startPcUpperPlusOne:  Option[UInt] = None,
      startPcUpperMinusOne: Option[UInt] = None
  ): UInt = {
    val startPcUpper = getTargetUpper(startPc)
    Cat(
      if (targetCarry.isDefined) // `if (EnableTargetFix)` in ubtb and abtb
        fixTargetUpper(startPcUpper, targetCarry.get, startPcUpperPlusOne, startPcUpperMinusOne)
      else
        startPcUpper,       // (VAddrBits - 1, TargetWidth + instOffsetBits)
      target,               // (TargetWidth + instOffsetBits - 1, instOffsetBits)
      0.U(instOffsetBits.W) // (instOffsetBits - 1, 0)
    )
  }
}
