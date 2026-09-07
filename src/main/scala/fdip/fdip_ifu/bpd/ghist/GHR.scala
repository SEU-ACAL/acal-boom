package boom.fdip.ifu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import boom.fdip.common._
import boom.fdip.util._

class GlobalHistory(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  // For the dual banked case, each bank ignores the contribution of the
  // last bank to the history. Thus we have to track the most recent update to the
  // history in that case
  val old_history = UInt(globalHistoryLength.W)



  def histories = {
    old_history
  }

  def ===(other: GlobalHistory): Bool = {
    (old_history === other.old_history) 
  }
  def =/=(other: GlobalHistory): Bool = !(this === other)
/**
 * @description: 全局历史更新函数
 * @param shamt: 当前有几个分支需要更新
 * @param endCfiTaken: 当前block的end分支是否taken
 * @param updateEnable: 是否需要更新全局历史
 * @func : GHR = GHR << shamt | Cat(0,endCfiTaken)(shamt.size,0)
 * @return: 更新后的全局历史
 */

//  def update(shamt: UInt, endCfiTaken: Bool, updateEnable: Bool): GlobalHistory = {
//     val new_history = Wire(new GlobalHistory)
//     val shamtUpdateBits = Cat(0.U(shamt.getWidth.W), endCfiTaken)
//     new_history.old_history := Mux(updateEnable, (histories << shamt) | shamtUpdateBits(shamt.getWidth, 0), histories)

//     new_history
//   }

  def update(shamt: UInt, endCfiTaken: Bool, endCfiIsCond: Bool, updateEnable: Bool): GlobalHistory = {
    val new_history = Wire(new GlobalHistory)
    val shamtUpdateBits = Mux(endCfiIsCond, Cat(0.U(shamt.getWidth.W), endCfiTaken), 0.U(shamt.getWidth.W))
    new_history.old_history := Mux(updateEnable, (histories << shamt) | shamtUpdateBits(shamt.getWidth, 0), histories)

    new_history
  }
}
