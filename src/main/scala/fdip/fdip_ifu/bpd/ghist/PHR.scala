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
class PHR(implicit p: Parameters) extends PHRModule with PhrHelpers {
    val io = IO(new Bundle{

        val s1Train = Input(new PhrS1Train)
        val phrMeta = Output(new PhrMeta)
        val train = Input(new PhrTrain)

        // val s1_foldPhr
    })
    val phr = RegInit(0.U(PHRWidth.W))
    val ptr = RegInit(0.U.asTypeOf(new PhrPtr(PHRWidth)))

    val s1_valid = io.s1Train.valid
    val s1_component = getPathHashComponents(io.s1Train.cfiPc, io.s1Train.target)
    val s1_shiftBits = s1_component._1
    val s1_hashHighBits = s1_component._2
    val s1_update

    val s2_override = io.train.s2.valid
    val s2_component = getPathHashComponents(io.train.s2.cifPc, io.train.s2.prediction.target)
    val s2_shiftBits = s2_component._1
    val s2_hashHighBits = s2_component._2

    val redirect = io.train.redirect.valid
    val redirect_component = getPathHashComponents(io.train.redirect.bits.cfiPc, io.train.redirect.bits.target)
    val redirect_shiftBits = redirect_component._1
    val redirect_hashHighBits = redirect_component._2

    val phrWriteValid = io.train.s2.valid || io.train.redirect.valid || io.s1Train.valid
    val phrWriteTaken = MuxCase(
        false.B,
        Seq(
            io.train.redirect.valid -> io.train.redirect.bits.taken,
            io.train.s2.valid -> io.train.s2.prediction.taken,
            io.s1Train.valid -> io.s1Train.taken
        ))
    val phrWriteBits  = MuxCase(
        0.U,
        Seq(
        io.train.redirect.valid -> Cat(redirect_shiftBits, redirect_hashHighBits),
        io.train.s2.valid -> Cat(s2_shiftBits, s2_hashHighBits),
        io.s1Train.valid -> Cat(s1_shiftBits, s1_hashHighBits)
    ))
    val phrWriteLowBits = MuxCase(
        0.U,
        Seq(
            io.train.redirect.valid -> redirect_hashHighBits,
            io.train.s2.valid -> s2_hashHighBits,
            io.s1Train.valid -> s1_hashHighBits
        ))
    val phrWritePtr = MuxCase(
        ptr,
        Seq(
            io.train.redirect.valid -> io.train.redirect.bits.phrMeta.ptr,
            io.train.s2.valid -> io.train.s2.phrMeta.ptr,
            io.s1Train.valid -> io.s1Train.phrMeta.ptr //TODO: fix this
        )
    )


    ptr := MuxCase(
        ptr,
        Seq(
            io.train.redirect.valid -> io.train.redirect.bits.meta.ptr,
            io.train.s2.valid -> io.train.s2.phrMeta.ptr,
            io.s1Train.valid -> Mux(io.s1Train.taken, ptr - 2.U, ptr)
        )
    )
    /*
    如果 taken 此时 ptr一定是已经更新过的值了(已经-2)，而ptr指示的是下一个要写的位置，比如 phr width为32
    s1发生taken，此时ptr为 30，其需要写入的位置为 31, 0, 1, 2, 3, 4, 5, 6, 7
    如果该预测在s2发生了override，s2保存的初始ptr为0，如果s2 taken(也就是s2 target或者其他和s1不同)，
    此时需要使用s2的新ptr 也就是30来写入

    如果s2为NT，此时ptr就是s2保存的ptr，也就是0，只需要恢复之前被s1写入的值，也就是 0, 1, 2, 3, 4, 5, 6, 7
    */

    when(phrWriteValid){
        when(phrWriteTaken){
            for(i <- 0 until PHRHashWidth){
                phr(phrWritePtr.value + (i+1).U) := phrWriteBits(i)
            }
        }.otherwise{
            for(i <- 1 to PHRHashHighWidth){
                phr(phrWritePtr.value + i.U) := phrWriteLowBits(i-1)
            }
        }
    }
}
