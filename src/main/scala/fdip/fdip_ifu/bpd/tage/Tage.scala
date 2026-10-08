package boom.fdip.ifu.bpd.tage

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.fdip.common._
import boom.fdip.util.{BoomCoreStringPrefix, MaskLower, WrapInc}
import boom.fdip.ifu._
import boom.fdip.ifu.bpd._
import boom.perf._

import scala.math.min
class TageTable(val nRows: Int, val tagSz: Int, val histLength: Int, val uBitPeriod: Int)
  (implicit p: Parameters) extends TageModule()(p)
  with HasBoomFrontendParameters
{
  require(histLength <= globalHistoryLength)

  val io = IO( new Bundle {
    val readReq = Input(Vec(TageNways, Valid(new TageTableReadReq())))
    val readResp = Output(Vec(TageNways,new TageTableReadResp()))

    val writeReq = Input(Valid(new TageTableWriteReq()))
    val writeEntryAccepted = Output(Vec(TageNways, Bool()))
    val writeUsefulAccepted = Output(Vec(TageNways, Bool()))
    val clearUseful = Output(Valid(UInt(log2Ceil(nRows).W)))
  })

  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nRows).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nRows-1).U) { doing_reset := false.B }



  val usefulCnt  = Seq.fill(TageNways){SyncReadMem(nRows, UInt(2.W))}
  val table      = Seq.fill(TageNways){SyncReadMem(nRows,new TageEntry)}
  



  for(i <- 0 until TageNways){
    val readEntryResp = table(i).read(io.readReq(i).bits.setIdx, io.readReq(i).valid)
    val readUsefulResp = usefulCnt(i).read(io.readReq(i).bits.setIdx, io.readReq(i).valid)
    io.readResp(i).valid := readEntryResp.valid
    io.readResp(i).ctr := readEntryResp.ctr
    io.readResp(i).tag := readEntryResp.tag
    io.readResp(i).u   := readUsefulResp
  }



  val clear_u_ctr = RegInit(0.U((log2Ceil(uBitPeriod) + log2Ceil(nRows) + 1).W))
  when (doing_reset) { clear_u_ctr := 1.U } .otherwise { clear_u_ctr := clear_u_ctr + 1.U }

  val doing_clear_u = clear_u_ctr(log2Ceil(uBitPeriod)-1,0) === 0.U
  val clear_u_idx = clear_u_ctr >> log2Ceil(uBitPeriod)
  io.clearUseful.valid := !doing_reset && doing_clear_u
  io.clearUseful.bits := clear_u_idx
  for(i <- 0 until TageNways) {
    io.writeEntryAccepted(i) := !doing_reset && io.writeReq.valid &&
      io.writeReq.bits.writeEntry(i) && io.writeReq.bits.wayMask(i)
    io.writeUsefulAccepted(i) := !doing_reset && !doing_clear_u &&
      io.writeReq.valid && io.writeReq.bits.writeUseful(i) && io.writeReq.bits.wayMask(i)

    when(doing_reset || doing_clear_u || (io.writeReq.valid && io.writeReq.bits.writeUseful(i) && io.writeReq.bits.wayMask(i))){
      usefulCnt(i).write(
        Mux(doing_reset, reset_idx, Mux(doing_clear_u, clear_u_idx, io.writeReq.bits.setIdx)),
        Mux(doing_reset || doing_clear_u, 0.U, io.writeReq.bits.u(i))
      )
    }
    when(doing_reset|| (io.writeReq.valid && io.writeReq.bits.writeEntry(i) && io.writeReq.bits.wayMask(i))){
      table(i).write(
        Mux(doing_reset, reset_idx, io.writeReq.bits.setIdx),
        Mux(doing_reset, 0.U.asTypeOf(new TageEntry), io.writeReq.bits.entry(i))
      )
    }
  }
}





class TageBranchPredictor(implicit p: Parameters) extends TageModule()(p)
{
  val params = tageParams


  val io = IO(new TagePredictorIO)
  val tageNTables = TageTables
  val tables = params.tableInfo map {
    case (n, l, s) => {
      val t = Module(new TageTable(n, s, l, params.uBitPeriod))
      t
    }
  }

  /** Simulator-only traces used to correlate TAGE lookup state with updates. */
  class TagePredictionTrace extends Bundle {
    val tableIdx   = UInt(log2Ceil(tageNTables max 2).W)
    val wayIdx     = UInt(log2Ceil(TageNways max 2).W)
    val startPc    = UInt(vaddrBitsExtended.W)
    val cfiPosition = UInt(log2Ceil(fetchBytes).W)
    // History and tag before the BTB position is mixed into the lookup tag.
    val ghist      = UInt(globalHistoryLength.W)
    val idx        = UInt(log2Ceil(params.tableInfo.map(_._1).max).W)
    val baseTag    = UInt(params.tagBits.W)
    val storedTag  = UInt(params.tagBits.W)
    val lookupTag  = UInt(params.tagBits.W)
    val hit        = Bool()
    val taken      = Bool()
    val ctr        = UInt(3.W)
    val valid      = Bool()
    val u          = UInt(2.W)
  }

  class TageTrainTrace extends Bundle {
    val tableIdx  = UInt(log2Ceil(tageNTables max 2).W)
    val wayIdx    = UInt(log2Ceil(TageNways max 2).W)
    val startPc   = UInt(vaddrBitsExtended.W)
    val cfiPosition = UInt(log2Ceil(fetchBytes).W)
    val ghist      = UInt(globalHistoryLength.W)
    val idx       = UInt(log2Ceil(params.tableInfo.map(_._1).max).W)
    val baseTag    = UInt(params.tagBits.W)
    val tag       = UInt(params.tagBits.W)
    // `taken` is the counter update direction; actualTaken is the branch direction.
    val actualTaken = Bool()
    val cfiValid  = Bool()
    val mispredict = Bool()
    val taken     = Bool()
    val ctr       = UInt(3.W)
    val valid     = Bool()
    val u         = UInt(2.W)
    val allocate  = Bool()
  }

  private val tagePredictionTrace = ChiselDB.createTable("TagePredictionTrace", new TagePredictionTrace)
  private val tageTrainTrace = ChiselDB.createTable("TageTrainTrace", new TageTrainTrace)

  def compute_folded_hist(histLength: Int, hist: UInt, l: Int) = {
    val nChunks = (histLength + l - 1) / l
    val hist_chunks = (0 until nChunks) map {i =>
      hist(min((i+1)*l, histLength)-1, i*l)
    }
    hist_chunks.reduce(_^_)
  }

  def compute_tag_and_hash(unhashed_idx: UInt, hist: UInt, tagSz: Int, nRows: Int, histLength: Int) = {
    val idx_history = compute_folded_hist(histLength, hist, log2Ceil(nRows))
    val idx = (unhashed_idx ^ idx_history)(log2Ceil(nRows)-1,0)
    val tag_history = compute_folded_hist(histLength, hist, tagSz)
    val tag = ((unhashed_idx >> log2Ceil(nRows)) ^ tag_history)(tagSz-1,0)
    (idx, tag)
  }

  def inc_ctr(ctr: UInt, taken: Bool): UInt = {
    Mux(!taken, Mux(ctr === 0.U, 0.U, ctr - 1.U),
                Mux(ctr === 7.U, 7.U, ctr + 1.U))
  }
  val s0_idx       = io.s0_pc>>instOffsetBits.U
  val s1_idx       = RegNext(s0_idx)
  val s2_idx       = RegNext(s1_idx)
  val s3_idx       = RegNext(s2_idx)


  val s0_valid     = io.s0_valid
  val s1_valid     = RegNext(s0_valid)
  val s2_valid     = RegNext(s1_valid)
  val s3_valid     = RegNext(s2_valid)

  val s0_pc        = io.s0_pc
  val s1_pc        = RegNext(s0_pc)
  val s2_pc        = RegNext(s1_pc)

	val tableResults = Wire(Vec(tables.size, Vec(TageNways, new TageTableReadResp())))

  io.tage_pred.taken.foreach { x =>
    x.valid := false.B
    x.bits := false.B
  }
  io.tage_pred.selectedEntry.foreach { x =>
    x := 0.U.asTypeOf(x)
  }
  io.meta := 0.U.asTypeOf(io.meta)
  def inc_u(u: UInt, inc: Bool): UInt = {
    Mux(inc, Mux(u === 3.U, 3.U, u + 1.U), u)
  }


  for(i <- 0 until tageNTables) {
    for(j <- 0 until TageNways) {
      tables(i).io.readReq(j).valid := s0_valid
      tables(i).io.readReq(j).bits.setIdx := compute_tag_and_hash(s0_idx, io.s0_ghist, params.tableInfo(i)._3, params.tableInfo(i)._1, params.tableInfo(i)._2)._1
      tableResults(i)(j) := tables(i).io.readResp(j)
    }
	}

  val s1_ghist = RegNext(io.s0_ghist)

	val s1_tag = WireInit(VecInit(io.fromBtb.s1_position.map{btb=>
    VecInit((0 until tageNTables).map{idx=>
    //TODO：优化这里的路径
			val (hash_idx, hash_tag) = compute_tag_and_hash(s1_idx, s1_ghist, params.tableInfo(idx)._3, params.tableInfo(idx)._1, params.tableInfo(idx)._2)
			hash_tag ^ btb
    })
	}))

  val s1_hashIdx = VecInit((0 until tageNTables).map { idx =>
    compute_tag_and_hash(
      s1_idx,
      s1_ghist,
      params.tableInfo(idx)._3,
      params.tableInfo(idx)._1,
      params.tableInfo(idx)._2
    )._1
  })
  val s2_hashIdx = RegNext(s1_hashIdx)
  val s2_ghist = RegNext(s1_ghist)

	val s2_tageResults = RegNext(tableResults)
	val s2_tag = RegNext(s1_tag)
	val s2_results = io.fromBtb.results

	s2_results.zipWithIndex.foreach { case (branch, i) =>
		val position =  branch.bits.cfiPosition
		val hitMask = VecInit(s2_tageResults.zipWithIndex.map{case(res,idx)=>
        VecInit((0 until TageNways).map{way=>
          res(way).valid && res(way).tag === s2_tag(i)(idx) && res(way).valid
        })
			})
		for (tableIdx <- 0 until tageNTables; wayIdx <- 0 until TageNways) {
			val traceEntry = s2_tageResults(tableIdx)(wayIdx)
			val tagePredictionTraceRecord = Wire(new TagePredictionTrace)
			tagePredictionTraceRecord.tableIdx := tableIdx.U
			tagePredictionTraceRecord.wayIdx := wayIdx.U
			tagePredictionTraceRecord.startPc := s2_pc
			tagePredictionTraceRecord.cfiPosition := position
			tagePredictionTraceRecord.ghist := s2_ghist
			tagePredictionTraceRecord.idx := s2_hashIdx(tableIdx)
			tagePredictionTraceRecord.baseTag := s2_tag(i)(tableIdx) ^ position
			tagePredictionTraceRecord.storedTag := traceEntry.tag
			tagePredictionTraceRecord.lookupTag := s2_tag(i)(tableIdx)
			tagePredictionTraceRecord.hit := hitMask(tableIdx)(wayIdx)
			tagePredictionTraceRecord.taken := traceEntry.ctr(2)
			tagePredictionTraceRecord.ctr := traceEntry.ctr
			tagePredictionTraceRecord.valid := traceEntry.valid
			tagePredictionTraceRecord.u := traceEntry.u
			tagePredictionTrace.log(
				tagePredictionTraceRecord,
				branch.valid && s2_valid,
				s"predict-t${tableIdx}-w${i}-way${wayIdx}"
			)(clock, reset)
		}
    val hasProvider = hitMask.map(_.reduce(_||_)).reduce(_||_)
    val providerHitTableMask = hitMask.map(_.reduce(_||_))
    val providerOH = PriorityEncoderOH(providerHitTableMask.reverse).reverse
    val providerIdx = OHToUInt(providerOH)
    val providerHitwayMask = VecInit(PriorityEncoderOH(hitMask(providerIdx)))
    val providerWayEntry = Mux1H(providerOH, s2_tageResults)
    val providerFinalEntry = Mux1H(providerHitwayMask, providerWayEntry)

    val hitWithoutProviderMask = VecInit(providerHitTableMask.zip(providerOH).map{case(h,p)=>h && !p})
    val hasAlt = hasProvider && hitWithoutProviderMask.reduce(_||_)
    val altOH = PriorityEncoderOH(hitWithoutProviderMask.reverse).reverse
    val altIdx = OHToUInt(altOH)
    val altHitwayMask = VecInit(PriorityEncoderOH(hitMask(altIdx)))
    val altWayEntry = Mux1H(altOH, s2_tageResults)
    val altFinalEntry = Mux1H(altHitwayMask, altWayEntry)

    //TODO: add alt sel
    val useProvider = hasProvider
    val providerTaken = providerFinalEntry.ctr(2)
    val altOrBasePred = Mux(hasAlt, altFinalEntry.ctr(2), branch.bits.taken)
    val selectedTableOH = VecInit(providerOH.zip(altOH).map { case (provider, alt) =>
      Mux(useProvider, provider, alt)
    })
    val selectedTableIdx = Mux(useProvider, providerIdx, altIdx)
    val selectedWayEntry = Mux1H(selectedTableOH, s2_tageResults)
    val selectHitwayMask = Mux(useProvider, providerHitwayMask, altHitwayMask)
    val selectedFinalEntry = Mux1H(selectHitwayMask, selectedWayEntry)
    val selectedLookupTag = Mux1H(selectedTableOH, s2_tag(i))
    val selectedIdx = Mux1H(selectedTableOH, s2_hashIdx)

    val preAllocTableMask = WireInit(VecInit(
      (0 until tageNTables).map{t=>
        VecInit((0 until TageNways).map{way=>
          (!s2_tageResults(t)(way).valid || s2_tageResults(t)(way).u === 0.U &&
          (s2_tageResults(t)(way).ctr === 3.U || s2_tageResults(t)(way).ctr === 4.U)) &&
          (t.U > providerIdx)
        })
      }
    ))
    io.tage_pred.taken(i).valid := useProvider || hasAlt
    io.tage_pred.taken(i).bits := Mux(useProvider, providerTaken, altFinalEntry.ctr(2))
    io.tage_pred.selectedEntry(i).valid := useProvider || hasAlt
    io.tage_pred.selectedEntry(i).tableIdx := selectedTableIdx
    io.tage_pred.selectedEntry(i).idx := selectedIdx
    io.tage_pred.selectedEntry(i).lookupTag := selectedLookupTag
    io.tage_pred.selectedEntry(i).storedTag := selectedFinalEntry.tag
    io.tage_pred.selectedEntry(i).ctr := selectedFinalEntry.ctr
    io.tage_pred.selectedEntry(i).ghist := s2_ghist
    io.meta.provider(i).valid := hasProvider
    io.meta.provider(i).bits  := OHToUInt(providerOH) 
    io.meta.alt_differs(i)    := altOrBasePred =/= providerTaken
    io.meta.provider_u(i)     := providerFinalEntry.u
    io.meta.provider_ctr(i)   := providerFinalEntry.ctr
    io.meta.allocate(i).valid := preAllocTableMask.asUInt.orR
    io.meta.allocate(i).bits  := VecInit(preAllocTableMask.map(_.asUInt))
    io.meta.provider_wayMask(i) := providerHitwayMask.asUInt
	}



/* 
Train Stage
 */
  val t0_valid = io.train.valid
  val t0_tageMeta = io.train.bits.tageMeta
  val t0_btbMeta = io.train.bits.btbMeta
  val t0_br_mask = io.train.bits.brMask
  val t0_cfi = io.train.bits.cfi
  val t0_pc = io.train.bits.startPc
  val t0_instOffset = t0_pc(log2Ceil(fetchBytes)-1, 1)
  val t0_cfiInRange = t0_cfi.valid && t0_cfi.position >= t0_instOffset
  val t0_ghist = io.train.bits.phr
  val t0_idx = (t0_pc)>>instOffsetBits.U
  
  // val t0_providerOH = t0_tageMeta.provider(t0_cfi.position)

  val tageUBitPeriod = TageUsefulPeriod
  val t0_hash = (0 until tageNTables).map { tableIdx =>
    compute_tag_and_hash(
      t0_idx,
      t0_ghist,
      params.tableInfo(tableIdx)._3,
      params.tableInfo(tableIdx)._1,
      params.tableInfo(tableIdx)._2
    )
  }
  val t0_hashIdx = VecInit(t0_hash.map(_._1))
  val t0_hashTag = VecInit(t0_hash.map(_._2))
  // FIFO of write events; entry 0 is newest and includes all concurrent tables.
  val ctrWriteBypass = RegInit(VecInit(Seq.fill(params.writeBypassEntries)(
    0.U.asTypeOf(new TageWriteBypass))))
  val t0_providerState = t0_tageMeta.provider.zip(t0_btbMeta).zipWithIndex.map {
    case ((provider, btb), idx) =>
      val tableIdx = provider.bits
      val providerWayMask = t0_tageMeta.provider_wayMask(idx)
      val bypassWayHits = ctrWriteBypass.map { entry =>
        VecInit((0 until TageNways).map { way =>
          provider.valid && providerWayMask(way) && entry.valid(tableIdx)(way) &&
            entry.idx(tableIdx)(way) === t0_hashIdx(tableIdx) &&
            entry.tag(tableIdx)(way) === (t0_hashTag(tableIdx) ^ btb.bits.position)
        })
      }
      val bypassHits = VecInit(bypassWayHits.map(_.reduce(_ || _)))
      val bypassCtrByEntry = ctrWriteBypass.zip(bypassWayHits).map { case (entry, entryWayHit) =>
        Mux1H(entryWayHit, VecInit((0 until TageNways).map(way => entry.ctr(tableIdx)(way))))
      }
      // Counter-only writes must not hide an older useful-counter write.
      val bypassUWayHits = ctrWriteBypass.zip(bypassWayHits).map { case (entry, entryWayHit) =>
        VecInit((0 until TageNways).map(way => entryWayHit(way) && entry.uValid(tableIdx)(way)))
      }
      val bypassUHits = VecInit(bypassUWayHits.map(_.reduce(_ || _)))
      val bypassUByEntry = ctrWriteBypass.zip(bypassUWayHits).map { case (entry, entryWayHit) =>
        Mux1H(entryWayHit, VecInit((0 until TageNways).map(way => entry.u(tableIdx)(way))))
      }
      val bypassCtr = Mux1H(PriorityEncoderOH(bypassHits), bypassCtrByEntry)
      val bypassU = Mux1H(PriorityEncoderOH(bypassUHits), bypassUByEntry)
      (Mux(bypassHits.asUInt.orR, bypassCtr, t0_tageMeta.provider_ctr(idx)),
        Mux(bypassUHits.asUInt.orR, bypassU, t0_tageMeta.provider_u(idx)))
  }
  val t0_providerCtr = VecInit(t0_providerState.map(_._1))
  val t0_providerU = VecInit(t0_providerState.map(_._2))
    /* 
    mispred的项: 首先看是否hitBTB，如果hit 并且有provider
      1. 如果provider是最长的，那么不需要allocate
      2. 如果provider不是最长的，那么需要allocate
      3. provider的Ctr同样需要更新
      4. 如果没有provider，那么需要allocate第一个表
      5. allocate 可以randrom alloc,高位表随机选一个 Ctr weak u=0的 或者！valid
    对于正常的项:
      1.hitBTB并且有provider，那么更新provider的Ctr和U
      2.如果预测是alt给出的，更新alt的Ctr
    allocate和正常更新的table撞了如何解决?
     */
    val t0_mispredHitTableMask = t0_btbMeta.zipWithIndex.map{case(btb,btbIdx)=>
      val mispredHit = btb.valid && btb.bits.hit  && btb.bits.attribute.isConditional&& 
        t0_cfiInRange && t0_cfi.attribute.isConditional &&
        t0_cfi.mispredict && (btb.bits.position === t0_cfi.position)
      mispredHit
    }
    // which BTB entry is the provider of the mispredicted branch, if any
    val t0_mispredHitTableMetaIdx = PriorityEncoder(t0_mispredHitTableMask)
    val t0_mispredHitTableMeta = t0_tageMeta.provider(t0_mispredHitTableMetaIdx)
    val t0_mispredHasProvider = t0_mispredHitTableMask.reduce(_||_) && t0_mispredHitTableMeta.valid
    //provider是不是最长的，此时无法分配
    val t0_providerIsLongest = t0_mispredHasProvider && (t0_mispredHitTableMeta.bits === (TageTables - 1).U)
    //TODO:allocate 算法可以优化
    val t0_needAllocate = t0_cfiInRange && t0_cfi.attribute.isConditional && t0_cfi.mispredict && !t0_providerIsLongest
    val t0_canAllocate = t0_tageMeta.allocate(t0_mispredHitTableMetaIdx).valid || !t0_mispredHitTableMask.reduce(_||_)
    val t0_randAlloc = random.LFSR(width = 15)(tageNTables - 1, 0)
    val t0_allocWayMasks = t0_tageMeta.allocate(t0_mispredHitTableMetaIdx).bits
    val t0_allocTableMask = VecInit(t0_allocWayMasks.map(_.orR)).asUInt
    val t0_randAllocMask = t0_randAlloc & t0_allocTableMask
    val t0_finalAllocMask  = Mux(t0_randAllocMask.orR,t0_randAllocMask,t0_allocTableMask)
    val t0_allocateTableIdx = Mux(
      t0_mispredHitTableMask.reduce(_||_) && t0_tageMeta.allocate(t0_mispredHitTableMetaIdx).valid,
      PriorityEncoder(t0_finalAllocMask),
      0.U)
    val t0_randAllocWayWidth = log2Ceil(TageNways max 2)
    val t0_randAllocWay = random.LFSR(width = t0_randAllocWayWidth max 2)(t0_randAllocWayWidth - 1, 0)
    val t0_randAllocWayMask = UIntToOH(t0_randAllocWay, TageNways)
    val t0_allocateWayMask = Mux(t0_mispredHitTableMask.reduce(_||_) && t0_tageMeta.allocate(t0_mispredHitTableMetaIdx).valid,
          PriorityEncoderOH(t0_allocWayMasks(t0_allocateTableIdx)),t0_randAllocWayMask)
    /* 
    CTR taken 111 和NT 000都不更新
    br_mask 可以得到不taken cfi可以得到taken 信息
     */
    val t0_providerUpdateInfo = (0 until tageNTables).map{i=>
      val t0_wayInfo = (0 until TageNways).map{way =>
        val updateInfo = t0_tageMeta.provider.zip(t0_btbMeta).zipWithIndex.map{case((p,btb),idx)=>

          val hitWay = t0_tageMeta.provider_wayMask(idx)(way)
          val hasProvider = t0_valid && p.valid && p.bits === i.U && hitWay
          // Entries before this request's start PC were not executed.
          val btbCondHit = t0_valid && btb.valid && btb.bits.hit &&
            btb.bits.attribute.isConditional && btb.bits.position >= t0_instOffset
          // A normal update is for a conditional branch before the resolved CFI.
          // If the block has no resolved CFI, every visible conditional branch is NT.
          val normalPosition = !t0_cfi.valid || btb.bits.position < t0_cfi.position
          val normalHit = hasProvider && btbCondHit && normalPosition
          // The end-CFI update must refer to a real conditional BTB hit; comparing
          // position alone can otherwise train an invalid way (position defaults 0).
          val endCfiHit = hasProvider && btbCondHit && t0_cfi.valid &&
            t0_cfi.attribute.isConditional && t0_cfi.position === btb.bits.position

          val normalNeedUpdateCtr = normalHit && t0_providerCtr(idx) =/= 0.U
          val normalIncCtr = normalHit && !normalNeedUpdateCtr

          val endCfiNeedUpdateCtr =  endCfiHit &&
          !(t0_providerCtr(idx) === 7.U && t0_cfi.taken ||
          t0_providerCtr(idx) === 0.U && !t0_cfi.taken)
          val endCfiIncCtr = endCfiNeedUpdateCtr && t0_cfi.taken


          //normal默认是不taken
          // Useful updates require a provider/BTB position match, disagreement
          // with the alternate predictor, a correct provider prediction, and U<3.
          val usefulProviderCandidate = hasProvider
          val usefulBtbCandidate = usefulProviderCandidate && btbCondHit
          val usefulPositionCandidate = usefulBtbCandidate && (normalPosition ||
            (t0_cfi.valid && t0_cfi.attribute.isConditional &&
              t0_cfi.position === btb.bits.position))
          val usefulAltCandidate = usefulPositionCandidate && t0_tageMeta.alt_differs(idx)
          val usefulCorrectCandidate = usefulAltCandidate && Mux(
            endCfiHit,
            t0_tageMeta.provider_ctr(idx)(2) === t0_cfi.taken,
            !t0_tageMeta.provider_ctr(idx)(2))
          val usefulNotSaturatedCandidate = usefulCorrectCandidate &&
            t0_providerU(idx) =/= 3.U
          val normalNeedUpdateUseful = usefulNotSaturatedCandidate && normalHit
          val endCfiNeedUpdateUseful = usefulNotSaturatedCandidate && endCfiHit

          (endCfiNeedUpdateCtr || normalNeedUpdateCtr,
            endCfiIncCtr || normalIncCtr,
            endCfiNeedUpdateUseful || normalNeedUpdateUseful,
            normalHit || endCfiHit,
            usefulProviderCandidate,
            usefulBtbCandidate,
            usefulPositionCandidate,
            usefulAltCandidate,
            usefulCorrectCandidate,
            usefulNotSaturatedCandidate)
        }
        val needUpdateCtr = VecInit(updateInfo.map(_._1))
        val incCtr        = VecInit(updateInfo.map(_._2))
        val updateUseful  = VecInit(updateInfo.map(_._3))
        val hits             = VecInit(updateInfo.map(_._4))
        val hitPosition = Mux1H(hits, t0_btbMeta.map(_.bits.position))
        (needUpdateCtr.reduce(_||_), incCtr.reduce(_||_), updateUseful.reduce(_||_),hits, hitPosition,
          VecInit(updateInfo.map(_._5)), VecInit(updateInfo.map(_._6)),
          VecInit(updateInfo.map(_._7)), VecInit(updateInfo.map(_._8)),
          VecInit(updateInfo.map(_._9)), VecInit(updateInfo.map(_._10)))
      }
      t0_wayInfo
    }
    val t0_providerUpdateNeed = t0_providerUpdateInfo.map { ways =>
      VecInit(ways.map(_._1))
    }
    val t0_providerUpdateInc = t0_providerUpdateInfo.map { ways =>
      VecInit(ways.map(_._2))
    }
    val t0_providerUpdateUseful = t0_providerUpdateInfo.map { ways =>
      VecInit(ways.map(_._3))
    }
    val t0_hitBtbVec = t0_providerUpdateInfo.map { ways =>
      ways.map(_._4)
    }
    val t0_hitPosition = t0_providerUpdateInfo.map { ways =>
      VecInit(ways.map(_._5))
    }
    for (i <- 0 until tageNTables) {
      val allocTable = t0_needAllocate && t0_canAllocate &&
        t0_allocateTableIdx === i.U
      val writeWayMask = VecInit((0 until TageNways).map { way =>
        val doAlloc = allocTable && t0_allocateWayMask(way)
        val doUpdate = t0_providerUpdateNeed(i)(way) ||
          t0_providerUpdateUseful(i)(way)
        doAlloc || doUpdate
      })

      tables(i).io.writeReq.valid := t0_valid && writeWayMask.asUInt.orR
      tables(i).io.writeReq.bits.setIdx := t0_hashIdx(i)
      tables(i).io.writeReq.bits.wayMask := writeWayMask

      for (j <- 0 until TageNways) {
        val doAlloc = allocTable && t0_allocateWayMask(j)
        val doUpdate = t0_providerUpdateNeed(i)(j) ||
          t0_providerUpdateUseful(i)(j)
        val writeEntryEn = t0_valid && (doAlloc || doUpdate)
        val hitBtbVec = t0_hitBtbVec(i)(j)
        val hitBtb = hitBtbVec.reduce(_ || _)
        val hitTageCtr = Mux1H(hitBtbVec, t0_providerCtr)
        val hitTageU = Mux1H(hitBtbVec, t0_providerU)
        val hitPosition = t0_hitPosition(i)(j)
        val hitNewCtr = Mux(
          t0_providerUpdateNeed(i)(j),
          inc_ctr(hitTageCtr, t0_providerUpdateInc(i)(j)),
          hitTageCtr)
        val hitNewU = inc_u(hitTageU, t0_providerUpdateUseful(i)(j))
        val trainPosition = Mux(doAlloc, t0_cfi.position, hitPosition)
        val trainIsEndCfi = t0_cfi.valid &&
          t0_cfi.attribute.isConditional && hitBtb &&
          hitPosition === t0_cfi.position
        val trainActualTaken = Mux(
          doAlloc,
          t0_cfi.taken,
          trainIsEndCfi && t0_cfi.taken)

        tables(i).io.writeReq.bits.writeEntry(j)   := writeEntryEn
        tables(i).io.writeReq.bits.writeUseful(j)  := t0_valid &&
          (doAlloc || t0_providerUpdateUseful(i)(j))
        tables(i).io.writeReq.bits.entry(j).valid  := writeEntryEn
        tables(i).io.writeReq.bits.entry(j).tag    :=
          t0_hashTag(i) ^ Mux(doAlloc, t0_cfi.position, hitPosition)
        tables(i).io.writeReq.bits.entry(j).ctr    :=
          Mux(doAlloc, Mux(t0_cfi.taken, 4.U, 3.U), hitNewCtr)
        tables(i).io.writeReq.bits.u(j)            :=
          Mux(doAlloc, 0.U(2.W), hitNewU)
        val writeReq = tables(i).io.writeReq
        val tageTrainTraceRecord = Wire(new TageTrainTrace)
        tageTrainTraceRecord.tableIdx := i.U
        tageTrainTraceRecord.wayIdx := j.U
        tageTrainTraceRecord.startPc := t0_pc
        tageTrainTraceRecord.cfiPosition := trainPosition
        tageTrainTraceRecord.ghist := t0_ghist
        tageTrainTraceRecord.idx := t0_hashIdx(i)
        tageTrainTraceRecord.baseTag := t0_hashTag(i)
        tageTrainTraceRecord.tag := writeReq.bits.entry(j).tag
        tageTrainTraceRecord.actualTaken := trainActualTaken
        tageTrainTraceRecord.cfiValid := doAlloc || trainIsEndCfi
        tageTrainTraceRecord.mispredict := doAlloc ||
          (trainIsEndCfi && t0_cfi.mispredict)
        tageTrainTraceRecord.taken := Mux(
          doAlloc, t0_cfi.taken, t0_providerUpdateInc(i)(j))
        tageTrainTraceRecord.ctr := writeReq.bits.entry(j).ctr
        tageTrainTraceRecord.valid := writeReq.bits.entry(j).valid
        tageTrainTraceRecord.u := writeReq.bits.u(j)
        tageTrainTraceRecord.allocate := doAlloc
        tageTrainTrace.log(
          tageTrainTraceRecord,
          writeReq.valid && writeReq.bits.writeEntry(j) &&
            writeReq.bits.wayMask(j),
          s"train-write-t${i}-way${j}"
        )(clock, reset)
      }
    }

    // Keep cached U values coherent with periodic SRAM clears, including shifts.
    val clearedWriteBypass = WireInit(ctrWriteBypass)
    for (entry <- 0 until params.writeBypassEntries;
         i <- 0 until tageNTables; j <- 0 until TageNways) {
      when(tables(i).io.clearUseful.valid && ctrWriteBypass(entry).valid(i)(j) &&
        ctrWriteBypass(entry).idx(i)(j) === tables(i).io.clearUseful.bits) {
        clearedWriteBypass(entry).uValid(i)(j) := true.B
        clearedWriteBypass(entry).u(i)(j) := 0.U
      }
    }
    ctrWriteBypass := clearedWriteBypass
    val ctrWrites = VecInit(tables.map(_.io.writeEntryAccepted.asUInt.orR))
    when (ctrWrites.asUInt.orR) {
      for (entry <- 1 until params.writeBypassEntries) {
        ctrWriteBypass(entry) := clearedWriteBypass(entry - 1)
      }
      for (i <- 0 until tageNTables) {
        val writeReq = tables(i).io.writeReq.bits
        for (j <- 0 until TageNways) {
          val clearThisRow = tables(i).io.clearUseful.valid &&
            writeReq.setIdx === tables(i).io.clearUseful.bits
          ctrWriteBypass(0).valid(i)(j) :=
            tables(i).io.writeEntryAccepted(j)
          ctrWriteBypass(0).idx(i)(j) := writeReq.setIdx
          ctrWriteBypass(0).tag(i)(j) := writeReq.entry(j).tag
          ctrWriteBypass(0).ctr(i)(j) := writeReq.entry(j).ctr
          ctrWriteBypass(0).uValid(i)(j) := tables(i).io.writeUsefulAccepted(j) ||
            (tables(i).io.writeEntryAccepted(j) && clearThisRow)
          ctrWriteBypass(0).u(i)(j) := Mux(clearThisRow, 0.U, writeReq.u(j))
        }
      }
    }

    XSPerfAccumulate("tage_train", t0_valid)
    XSPerfAccumulate("tage_train_cfi", t0_valid && t0_cfi.valid)
    XSPerfAccumulate("tage_train_cond", t0_valid && t0_cfi.valid && t0_cfi.attribute.isConditional)
    XSPerfAccumulate("tage_train_mispred", t0_valid && t0_cfi.valid && t0_cfi.mispredict)
    XSPerfAccumulate("tage_provider_hit", Mux(t0_valid, PopCount(t0_tageMeta.provider.map(_.valid)), 0.U))
    XSPerfAccumulate("tage_provider_longest", t0_valid && t0_providerIsLongest)
    XSPerfAccumulate("tage_allocate_req", t0_valid && t0_needAllocate)
    XSPerfAccumulate("tage_allocate_do", t0_valid && t0_needAllocate && t0_canAllocate)
    val t0_providerUpdateNeedFlat =
      t0_providerUpdateNeed.flatMap(v => (0 until TageNways).map(v(_)))
    val t0_providerUpdateUsefulFlat =
      t0_providerUpdateUseful.flatMap(v => (0 until TageNways).map(v(_)))
    val t0_hitBtbFlat = t0_hitBtbVec.flatMap { ways =>
      ways.flatMap { branches =>
        branches.indices.map(branches(_))
      }
    }
    val t0_uCandidateProvider = t0_providerUpdateInfo.flatMap { ways =>
      ways.flatMap { info => info._6.indices.map(k => info._6(k)) }
    }
    val t0_uCandidateBtbCond = t0_providerUpdateInfo.flatMap { ways =>
      ways.flatMap { info => info._7.indices.map(k => info._7(k)) }
    }
    val t0_uCandidatePosition = t0_providerUpdateInfo.flatMap { ways =>
      ways.flatMap { info => info._8.indices.map(k => info._8(k)) }
    }
    val t0_uCandidateAltDiffers = t0_providerUpdateInfo.flatMap { ways =>
      ways.flatMap { info => info._9.indices.map(k => info._9(k)) }
    }
    val t0_uCandidateCorrect = t0_providerUpdateInfo.flatMap { ways =>
      ways.flatMap { info => info._10.indices.map(k => info._10(k)) }
    }
    val t0_uCandidateNotSaturated = t0_providerUpdateInfo.flatMap { ways =>
      ways.flatMap { info => info._11.indices.map(k => info._11(k)) }
    }
    XSPerfAccumulate("tage_provider_ctr_update", Mux(t0_valid, PopCount(t0_providerUpdateNeedFlat), 0.U))
    XSPerfAccumulate("tage_provider_u_update", Mux(t0_valid, PopCount(t0_providerUpdateUsefulFlat), 0.U))
    XSPerfAccumulate("tage_provider_hit_entries", Mux(t0_valid, PopCount(t0_hitBtbFlat), 0.U))
    XSPerfAccumulate("tage_u_candidate_provider", Mux(t0_valid, PopCount(t0_uCandidateProvider), 0.U))
    XSPerfAccumulate("tage_u_candidate_btb_cond", Mux(t0_valid, PopCount(t0_uCandidateBtbCond), 0.U))
    XSPerfAccumulate("tage_u_candidate_position", Mux(t0_valid, PopCount(t0_uCandidatePosition), 0.U))
    XSPerfAccumulate("tage_u_candidate_alt_differs", Mux(t0_valid, PopCount(t0_uCandidateAltDiffers), 0.U))
    XSPerfAccumulate("tage_u_candidate_correct", Mux(t0_valid, PopCount(t0_uCandidateCorrect), 0.U))
    XSPerfAccumulate("tage_u_candidate_not_saturated", Mux(t0_valid, PopCount(t0_uCandidateNotSaturated), 0.U))
}
