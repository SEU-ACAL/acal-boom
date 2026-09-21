package boom.fdip.ifu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import boom.fdip.common._
import boom.fdip.util._
/*
PHR 需要考虑：
1. 算法
2. 如何更新
3. 如何重定向
4. 如何
*/
class PHR(implicit p: Parameters) extends PHRModule{
    val io = IO(new Bundle{

    })
    val phr = RegInit(0.U(PHRWidth.W))

}
