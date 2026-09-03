package boom.fdip.ifu

import chisel3._
import chisel3.util._
import chisel3.util.random._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import freechips.rocketchip.rocket.{HasL1ICacheParameters, ICacheParams, ICacheErrors, ICacheReq}
import boom.fdip.ifu._



import boom.fdip.common._
import boom.fdip.util._
import freechips.rocketchip.regmapper.RRTest0Map.re


class WayLookup(implicit p: Parameters) extends ICacheModule{
    val io = IO(new Bundle{
        val write = Flipped(DecoupledIO(new WayLookupWrite))
        //Now only backend and ifu decode can flush it.BPU only has two stages, so it can not flush the waylookup
        val flush = Input(Bool())
        //1. evict 2. refill
        val update = Flipped(ValidIO(new ICacheMissResp))
        val mainPipeToWayLookup = Input(Bool())
        val wayLookupToMainPipe = DecoupledIO(new WayLookupToMainPipe)
    })

    val entry = RegInit(VecInit(Seq.fill(WayLookupEntrySz)(0.U.asTypeOf(new WayLookupEntry))))

    val writePtr = RegInit(0.U((log2Ceil(WayLookupEntrySz)+1).W))
    val readPtr = RegInit(0.U((log2Ceil(WayLookupEntrySz)+1).W))
    val writePtrValue = writePtr(log2Ceil(WayLookupEntrySz)-1,0)
    val readPtrValue = readPtr(log2Ceil(WayLookupEntrySz)-1,0)
    val writePtrFlag = writePtr(log2Ceil(WayLookupEntrySz))
    val readPtrFlag = readPtr(log2Ceil(WayLookupEntrySz))

    val full = writePtrValue === readPtrValue && writePtrFlag =/= readPtrFlag
    val empty = writePtrValue === readPtrValue && writePtrFlag === readPtrFlag
    val updateStall = WireInit(VecInit(Seq.fill(WayLookupEntrySz)(false.B)))

    io.write.ready := !full && !io.flush
    io.wayLookupToMainPipe.valid := !empty && !updateStall(readPtrValue) && !io.flush
    io.wayLookupToMainPipe.bits.entry := entry(readPtrValue)


    when(io.flush){
        writePtr := 0.U
        readPtr := 0.U
    }

    when(io.write.fire){
        entry(writePtr) := io.write.bits.entry
        writePtr := writePtr + 1.U
    }
    when(io.wayLookupToMainPipe.fire){
        readPtr := readPtr + 1.U
    }

    (entry zip updateStall).map{case (e, stall) =>
        when(io.update.valid){
            val setSame = io.update.bits.vSetIdx === e.vSetIdx
            val tagSame = io.update.bits.blkPaddr >> blockOffBits.U === e.pTag
            val waySame = UIntToOH(io.update.bits.way) === e.waymask
            when(setSame){
                when(tagSame){
                    e.waymask := UIntToOH(io.update.bits.way)
                }.elsewhen(waySame){
                    e.waymask := 0.U
                }
                stall := true.B
            }
        }
    }

    when(io.update.valid){
        val updateEntry = WireInit(0.U.asTypeOf(new WayLookupEntry))
        updateEntry.vSetIdx := io.update.bits.vSetIdx
        updateEntry.waymask := (1.U << io.update.bits.way)
        updateEntry.pTag := io.update.bits.blkPaddr >> blockOffBits.U

        entry(readPtr) := updateEntry
        readPtr := readPtr + 1.U
    }

}
