package boom.fdip.ifu.bpd

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import boom.fdip.common._
import boom.fdip.util._
import boom.perf.ChiselDB
/*
PHR 需要考虑：
1. 算法
2. 如何更新
3. 如何重定向
4. 是否需要维护Fold PHR的状态，如果PHR时序紧张，面积紧张 需要使用fold PHR
*/
class PHR(implicit p: Parameters) extends PHRModule with PhrHelpers {
    class PhrRedirectTrace extends Bundle {
        val cfiPc = UInt(vaddrBitsExtended.W)
        val target = UInt(vaddrBitsExtended.W)
        val taken = Bool()
        val s1Valid = Bool()
        val s2Valid = Bool()
        val ptrBefore = UInt(PHRPtrWidth.W)
        val checkpointPtr = UInt(PHRPtrWidth.W)
        val checkpointLowBits = UInt(PHRHashHighWidth.W)
        val restoredPtr = UInt(PHRPtrWidth.W)
        val restoredLowBits = UInt(PHRHashHighWidth.W)
        val hashLow = UInt(PHRShamt.W)
        val hashHigh = UInt(PHRHashHighWidth.W)
        val historyBefore = UInt(globalHistoryLength.W)
        val historyAtCheckpoint = UInt(globalHistoryLength.W)
        val historyAtRestoredPtr = UInt(globalHistoryLength.W)
    }
    class PhrTrainTrace extends Bundle {
        val startPc = UInt(vaddrBitsExtended.W)
        val cfiPosition = UInt(log2Ceil(fetchBytes).W)
        val target = UInt(vaddrBitsExtended.W)
        val taken = Bool()
        val checkpointPtr = UInt(PHRPtrWidth.W)
        val updatedPtr = UInt(PHRPtrWidth.W)
        val historyAtCheckpoint = UInt(globalHistoryLength.W)
        // The history reconstructed from the checkpoint and used for TAGE train.
        // This is a combinational view; it does not modify the live PHR state.
        val historyAtTrainLookup = UInt(globalHistoryLength.W)
        val historyAtUpdatedPtr = UInt(globalHistoryLength.W)
    }
    class PhrPostRedirectTrace extends Bundle {
        val cfiPc = UInt(vaddrBitsExtended.W)
        val expectedPtr = UInt(PHRPtrWidth.W)
        val actualPtr = UInt(PHRPtrWidth.W)
        val history = UInt(globalHistoryLength.W)
    }
    private val redirectTrace = ChiselDB.createTable("PhrRedirectTrace", new PhrRedirectTrace)
    private val trainTrace = ChiselDB.createTable("PhrTrainTrace", new PhrTrainTrace)
    private val postRedirectTrace = ChiselDB.createTable("PhrPostRedirectTrace", new PhrPostRedirectTrace)

    val io = IO(new Bundle{
        val s0_fire = Input(Bool())
        val s1_fire = Input(Bool())
        val s2_fire = Input(Bool())

        val s1Train = Input(new PhrS1Train)
        val phrMeta = Output(new PhrMeta)
        val train   = Input(new PhrTrain)
        val s0_phr = Output(UInt(PHRWidth.W))
        
        val train_phr = Output(UInt(PHRWidth.W))
        // val s1_foldPhr
    })
    val phr = RegInit(VecInit(Seq.fill(PHRWidth)(false.B)))
    val ptr = RegInit(0.U.asTypeOf(new PhrPtr(PHRWidth)))
    def readLogical(ptr: PhrPtr): UInt = {
        (Cat(phr.asUInt, phr.asUInt) >> (ptr.value + 1.U))(PHRWidth - 1, 0)
    }
    val s0_ptr = Wire(new PhrPtr(PHRWidth))
    val s0_fire = io.s0_fire
    val s1_fire = io.s1_fire
    val s2_fire = io.s2_fire
    val s0_phrBeforeWrite = readLogical(s0_ptr)
    val s0_phrValue = WireInit(s0_phrBeforeWrite)
    io.s0_phr := s0_phrValue

    val s1_valid = io.s1Train.valid
    val s1_Data = Wire(new PhrUpdate)
    val s1_ptr = RegEnable(s0_ptr, s0_fire)
    // Save the exact lookup snapshot, including any same-cycle write bypass.
    // Rereading the live array in s1 can observe a different history version.
    val s1_phrLowBits = RegEnable(s0_phrValue(PHRHashHighWidth-1,0), s0_fire)
    s1_Data.valid := s1_valid
    s1_Data.cifPc := io.s1Train.cfiPc
    s1_Data.phrMeta := io.phrMeta
    s1_Data.prediction := DontCare
    s1_Data.prediction.taken := io.s1Train.taken
    s1_Data.prediction.target := io.s1Train.target

    val s1_component = getPathHashComponents(io.s1Train.cfiPc, io.s1Train.target)
    val s1_shiftBits = s1_component._1
    val s1_hashHighBits = s1_component._2
    val s1_update = getUpdatePtrs(s1_Data,s1_hashHighBits)

    val s2_override = io.train.s2.valid
    val s2_component = getPathHashComponents(io.train.s2.cifPc, io.train.s2.prediction.target)
    val s2_shiftBits = s2_component._1
    val s2_hashHighBits = s2_component._2
    val s2_Data = Wire(new PhrUpdate)
    s2_Data.valid := s2_override
    s2_Data.cifPc := io.train.s2.cifPc
    s2_Data.phrMeta := io.train.s2.phrMeta
    s2_Data.prediction := io.train.s2.prediction
    val s2_update = getUpdatePtrs(s2_Data,s2_hashHighBits)


    val redirect = io.train.redirect.valid
    val redirect_component = getPathHashComponents(io.train.redirect.bits.cfiPc, io.train.redirect.bits.target)
    val redirect_shiftBits = redirect_component._1
    val redirect_hashHighBits = redirect_component._2
    val redirect_data = Wire(new PhrUpdate)
    redirect_data.valid := redirect
    redirect_data.cifPc := io.train.redirect.bits.cfiPc
    redirect_data.phrMeta := io.train.redirect.bits.phrMeta
    redirect_data.prediction := DontCare // 其他字段不care
    redirect_data.prediction.taken := io.train.redirect.bits.taken
    redirect_data.prediction.target := io.train.redirect.bits.target

    val redirect_update = getUpdatePtrs(redirect_data,redirect_hashHighBits)

    val redirectTraceRecord = Wire(new PhrRedirectTrace)
    redirectTraceRecord.cfiPc := redirect_data.cifPc
    redirectTraceRecord.target := redirect_data.prediction.target
    redirectTraceRecord.taken := redirect_data.prediction.taken
    redirectTraceRecord.s1Valid := io.s1Train.valid
    redirectTraceRecord.s2Valid := io.train.s2.valid
    redirectTraceRecord.ptrBefore := ptr.value
    redirectTraceRecord.checkpointPtr := redirect_data.phrMeta.ptr.value
    redirectTraceRecord.checkpointLowBits := redirect_data.phrMeta.phrLowBits
    redirectTraceRecord.restoredPtr := redirect_update.ptr.value
    redirectTraceRecord.restoredLowBits := redirect_update.phrLowBits
    redirectTraceRecord.hashLow := redirect_shiftBits
    redirectTraceRecord.hashHigh := redirect_hashHighBits
    redirectTraceRecord.historyBefore := readLogical(ptr)(globalHistoryLength-1, 0)
    redirectTraceRecord.historyAtCheckpoint := readLogical(redirect_data.phrMeta.ptr)(globalHistoryLength-1, 0)
    redirectTraceRecord.historyAtRestoredPtr := readLogical(redirect_update.ptr)(globalHistoryLength-1, 0)
    redirectTrace.log(redirectTraceRecord, redirect, "phr-redirect")(clock, reset)

    val postRedirectTraceRecord = Wire(new PhrPostRedirectTrace)
    postRedirectTraceRecord.cfiPc := RegNext(redirect_data.cifPc)
    postRedirectTraceRecord.expectedPtr := RegNext(redirect_update.ptr.value)
    postRedirectTraceRecord.actualPtr := ptr.value
    postRedirectTraceRecord.history := readLogical(ptr)(globalHistoryLength-1, 0)
    postRedirectTrace.log(postRedirectTraceRecord, RegNext(redirect, false.B), "phr-post-redirect")(clock, reset)

    val phrWriteValid = io.train.s2.valid || io.train.redirect.valid || io.s1Train.valid
    val phrWriteTaken = MuxCase(
        false.B,
        Seq(
            io.train.redirect.valid -> io.train.redirect.bits.taken,
            io.train.s2.valid -> io.train.s2.prediction.taken,
            io.s1Train.valid -> io.s1Train.taken
        ))
    val phrWriteLowBits = MuxCase(
        0.U,
        Seq(
            io.train.redirect.valid -> redirect_update.phrLowBits,
            io.train.s2.valid -> s2_update.phrLowBits,
            io.s1Train.valid -> s1_update.phrLowBits
        ))
    val phrWriteHashLowBits = MuxCase(
        0.U,
        Seq(
            io.train.redirect.valid -> redirect_shiftBits,
            io.train.s2.valid -> s2_shiftBits,
            io.s1Train.valid -> s1_shiftBits
        ))
    // A taken path appends the low hash bits and folds the high hash bits
    // into the low-history snapshot captured by the prediction.  Writing the
    // raw hash high bits here drops the prior snapshot and causes every taken
    // update to use stale history.  Keep this value shared by the array write
    // and the same-cycle s0 bypass.
    val phrWriteBits = Cat(phrWriteLowBits, phrWriteHashLowBits)
    val phrWritePtr = MuxCase(
        ptr,
        Seq(
            io.train.redirect.valid -> redirect_update.ptr,
            io.train.s2.valid -> s2_update.ptr,
            io.s1Train.valid -> s1_update.ptr
        )
    )
    ptr := phrWritePtr
    s0_ptr := phrWritePtr

    // s0 uses the selected post-update pointer before the register array is
    // written. Forward the same write into its logical low bits so lookup
    // observes the history that will exist after this clock edge. The write
    // starts at phrWritePtr + 1, which is logical bit 0 even across wraparound.
    when(phrWriteValid) {
        s0_phrValue := Mux(phrWriteTaken,
            Cat(s0_phrBeforeWrite(PHRWidth-1, PHRHashWidth), phrWriteBits),
            Cat(s0_phrBeforeWrite(PHRWidth-1, PHRHashHighWidth), phrWriteLowBits))
    }
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
                phr((phrWritePtr + (i+1).U).value) := phrWriteBits(i)
            }
        }.otherwise{
            for(i <- 1 to PHRHashHighWidth){
                phr((phrWritePtr + i.U).value) := phrWriteLowBits(i-1)
            }
        }
    }

    io.phrMeta.ptr := s1_ptr
    io.phrMeta.phrLowBits := s1_phrLowBits


    val bpuTrainTaken = io.train.bpuTrain.valid && io.train.bpuTrain.bits.bpuTraincfi.taken
    val bpuTrainData = Wire(new PhrUpdate)
    bpuTrainData := DontCare
    bpuTrainData.valid := io.train.bpuTrain.valid

    bpuTrainData.phrMeta := io.train.bpuTrain.bits.bpuTrainPhrMeta
    bpuTrainData.prediction.taken := io.train.bpuTrain.bits.bpuTraincfi.taken
    bpuTrainData.prediction.target := io.train.bpuTrain.bits.bpuTraincfi.target
    val bpuTrain_cifPc = fetchAlign(io.train.bpuTrain.bits.startPc) + (io.train.bpuTrain.bits.bpuTraincfi.position<<1.U)
    val bpuTrain_component = getPathHashComponents(bpuTrain_cifPc, io.train.bpuTrain.bits.bpuTraincfi.target)
    val bpuTrain_hashHighBits = bpuTrain_component._2
    // getUpdatePtrs() is still used by the speculative/redirect PHR paths above.
    // TAGE training, however, must use the history seen at lookup time.  Read
    // the saved checkpoint window and overlay its saved low bits on a temporary
    // value.  Neither the live PHR array nor the global pointer is modified.
    val bpuTrain_update = getUpdatePtrs(bpuTrainData,bpuTrain_hashHighBits)
    val bpuTrainCheckpointPhr = readLogical(bpuTrainData.phrMeta.ptr)
    val bpuTrainLookupPhr = Cat(
        bpuTrainCheckpointPhr(PHRWidth - 1, PHRHashHighWidth),
        bpuTrainData.phrMeta.phrLowBits
    )
    val bpuPhrValue = bpuTrainLookupPhr

    val trainTraceRecord = Wire(new PhrTrainTrace)
    trainTraceRecord.startPc := io.train.bpuTrain.bits.startPc
    trainTraceRecord.cfiPosition := io.train.bpuTrain.bits.bpuTraincfi.position
    trainTraceRecord.target := io.train.bpuTrain.bits.bpuTraincfi.target
    trainTraceRecord.taken := io.train.bpuTrain.bits.bpuTraincfi.taken
    trainTraceRecord.checkpointPtr := bpuTrainData.phrMeta.ptr.value
    trainTraceRecord.updatedPtr := bpuTrain_update.ptr.value
    trainTraceRecord.historyAtCheckpoint := bpuTrainCheckpointPhr(globalHistoryLength-1, 0)
    trainTraceRecord.historyAtTrainLookup := bpuTrainLookupPhr(globalHistoryLength-1, 0)
    trainTraceRecord.historyAtUpdatedPtr := readLogical(bpuTrain_update.ptr)(globalHistoryLength-1, 0)
    trainTrace.log(trainTraceRecord, io.train.bpuTrain.valid, "phr-train")(clock, reset)

    io.train_phr := bpuPhrValue(globalHistoryLength-1,0)
}
