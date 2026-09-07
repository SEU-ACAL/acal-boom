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
  (implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFrontendParameters
{
  require(histLength <= globalHistoryLength)

  val io = IO( new Bundle {
    val readReq = Input(Valid(new TageTableReadReq()))
    val readResp = Output(Valid(new TageTableReadResp()))

    val writeReq = Input(Valid(new TageTableWriteReq()))
  })

  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nRows).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nRows-1).U) { doing_reset := false.B }


  val usefulCnt  = SyncReadMem(nRows, UInt(2.W))
  val table      = SyncReadMem(nRows,new TageEntry)
  

  val readEntryResp = table.read(io.readReq.bits.setIdx, io.readReq.valid)
  val readUsefulResp = usefulCnt.read(io.readReq.bits.setIdx, io.readReq.valid)

  io.readResp.valid    := RegNext(io.readReq.valid)
  io.readResp.bits.ctr := readEntryResp.ctr
  io.readResp.bits.tag := readEntryResp.tag
  io.readResp.bits.u   := readUsefulResp


  val clear_u_ctr = RegInit(0.U((log2Ceil(uBitPeriod) + log2Ceil(nRows) + 1).W))
  when (doing_reset) { clear_u_ctr := 1.U } .otherwise { clear_u_ctr := clear_u_ctr + 1.U }

  val doing_clear_u = clear_u_ctr(log2Ceil(uBitPeriod)-1,0) === 0.U
  val clear_u_idx = clear_u_ctr >> log2Ceil(uBitPeriod)
  when(doing_reset || doing_clear_u || (io.writeReq.valid && io.writeReq.bits.writeUseful)){
    usefulCnt.write(
      Mux(doing_reset, reset_idx, Mux(doing_clear_u, clear_u_idx, io.writeReq.bits.setIdx)),
      Mux(doing_reset || doing_clear_u, 0.U, io.writeReq.bits.u)
    )
  }
	when(doing_reset|| (io.writeReq.valid && io.writeReq.bits.writeEntry)){
    table.write(
      Mux(doing_reset, reset_idx, Mux(doing_clear_u, clear_u_idx, io.writeReq.bits.setIdx)),
      Mux(doing_reset, 0.U.asTypeOf(new TageEntry), io.writeReq.bits.entry)
    )
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
  val s0_idx       = fetchIdx(io.s0_pc)
  val s1_idx       = RegNext(s0_idx)
  val s2_idx       = RegNext(s1_idx)
  val s3_idx       = RegNext(s2_idx)


  val s0_valid     = io.s0_valid
  val s1_valid     = RegNext(s0_valid)
  val s2_valid     = RegNext(s1_valid)
  val s3_valid     = RegNext(s2_valid)

  val s0_pc        = io.s0_pc
  val s1_pc        = RegNext(s0_pc)

	val tableResults = Wire(Vec(tables.size, Valid(new TageTableReadResp())))

  io.tage_pred.taken.foreach { x =>
    x.valid := false.B
    x.bits := false.B
  }
  io.meta := 0.U.asTypeOf(io.meta)
  def inc_u(u: UInt, inc: Bool): UInt = {
    Mux(inc, Mux(u === 3.U, 3.U, u + 1.U), u)
  }


  for(i <- 0 until tageNTables) {
		tables(i).io.readReq.valid := s0_valid
		tables(i).io.readReq.bits.setIdx := compute_tag_and_hash(s0_idx, io.s0_ghist, params.tableInfo(i)._3, params.tableInfo(i)._1, params.tableInfo(i)._2)._1
		tableResults(i) := tables(i).io.readResp
		
	}

  val s1_ghist = RegNext(io.s0_ghist)
	// s1_tag[btbNways][nTables] tag计算
	val s1_tag = WireInit(VecInit(io.fromBtb.s1_position.map{btb=>
		VecInit(tableResults.zipWithIndex.map{case(res,idx)=>
			//TODO：优化这里的路径
			val (hash_idx, hash_tag) = compute_tag_and_hash(s1_idx, s1_ghist, params.tableInfo(idx)._3, params.tableInfo(idx)._1, params.tableInfo(idx)._2)
			hash_tag ^ btb
		})
	}))

	val s2_tageResults = RegNext(tableResults)
	val s2_tag = RegNext(s1_tag)
	val s2_results = io.fromBtb.results

	s2_results.zipWithIndex.foreach { case (branch, i) =>
		val position =  branch.bits.cfiPosition
		val hitMask = VecInit(s2_tageResults.zipWithIndex.map{case(res,idx)=>
			res.valid && res.bits.tag === s2_tag(i)(idx)
		})
    val hasProvider = hitMask.reduce(_||_)
    val providerOH = PriorityEncoderOH(hitMask.reverse).reverse
    val providerIdx = OHToUInt(providerOH)
    val providerEntry = Mux1H(providerOH, s2_tageResults.map(_.bits))

    val hitWithoutProviderMask = VecInit(hitMask.zip(providerOH).map{case(h,p)=>h && !p})
    val hasAlt = hasProvider && hitWithoutProviderMask.reduce(_||_)
    val altOH = PriorityEncoderOH(hitWithoutProviderMask.reverse).reverse
    val altIdx = OHToUInt(altOH)
    val altEntry = Mux1H(altOH, s2_tageResults.map(_.bits))

    val useProvider = hasProvider && !(providerEntry.ctr===3.U || providerEntry.ctr===4.U)
    val providerTaken = providerEntry.ctr(2)
    val altOrBasePred = Mux(hasAlt, altEntry.ctr(2), branch.bits.taken)

    val preAllocTableMask = WireInit(VecInit(s2_tageResults.zipWithIndex.map{case(res,idx)=>
      (!res.valid || res.bits.u === 0.U && (res.bits.ctr === 3.U || res.bits.ctr === 4.U)) && (idx.U > providerIdx) 
    }))
    io.tage_pred.taken(i).valid := useProvider || hasAlt
    io.tage_pred.taken(i).bits := Mux(useProvider, providerTaken, altEntry.ctr(2))
    io.meta.provider(i).valid := hasProvider
    io.meta.provider(i).bits  := OHToUInt(providerOH) 
    io.meta.alt_differs(i)    := altOrBasePred =/= providerTaken
    io.meta.provider_u(i)     := providerEntry.u
    io.meta.provider_ctr(i)   := providerEntry.ctr
    io.meta.allocate(i).valid := preAllocTableMask.reduce(_||_)
    io.meta.allocate(i).bits  := preAllocTableMask.asUInt
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
  val t0_ghist = io.train.bits.ghist
  val t0_idx = fetchIdx(t0_pc)
  
  // val t0_providerOH = t0_tageMeta.provider(t0_cfi.position)

  val tageUBitPeriod = TageUsefulPeriod
  val t0_hash = (0 until tageNTables).map { tableIdx =>
    compute_tag_and_hash(
      t0_idx,
      t0_ghist.histories,
      params.tableInfo(tableIdx)._3,
      params.tableInfo(tableIdx)._1,
      params.tableInfo(tableIdx)._2
    )
  }
  val t0_hashIdx = VecInit(t0_hash.map(_._1))
  val t0_hashTag = VecInit(t0_hash.map(_._2))
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
        t0_cfi.valid && t0_cfi.attribute.isConditional && 
        t0_cfi.mispredict && (btb.bits.position === t0_cfi.position)
      mispredHit
    }
    val t0_mispredHitTableMetaIdx = PriorityEncoder(t0_mispredHitTableMask)
    val t0_mispredHitTableMeta = t0_tageMeta.provider(t0_mispredHitTableMetaIdx)
    val t0_mispredHasProvider = t0_mispredHitTableMask.reduce(_||_) && t0_mispredHitTableMeta.valid
    //provider是不是最长的，此时无法分配
    val t0_providerIsLongest = t0_mispredHasProvider && (t0_mispredHitTableMeta.bits === (TageTables - 1).U)
    //TODO:allocate 算法可以优化
    val t0_needAllocate = t0_cfi.valid && t0_cfi.attribute.isConditional && t0_cfi.mispredict && !t0_providerIsLongest 
    val t0_canAllocate = t0_tageMeta.allocate(t0_cfi.position).valid || !t0_mispredHitTableMask.reduce(_||_)
    val t0_allocateTableIdx = Mux(t0_mispredHitTableMask.reduce(_||_), t0_tageMeta.allocate(t0_mispredHitTableMetaIdx).bits, 0.U)

    /* 
    CTR taken 111 和NT 000都不更新
    br_mask 可以得到不taken cfi可以得到taken 信息
     */
    val t0_providerUpdateInfo = (0 until tageNTables).map{i=>
      
      val updateInfo = t0_tageMeta.provider.zip(t0_btbMeta).zipWithIndex.map{case((p,btb),idx)=>
        val hasProvider = t0_valid && p.valid && p.bits === i.U
        val btbCondHit = t0_valid && btb.valid && btb.bits.hit &&
          btb.bits.attribute.isConditional
        // A normal update is for a conditional branch before the resolved CFI.
        // If the block has no resolved CFI, every visible conditional branch is NT.
        val normalPosition = !t0_cfi.valid || btb.bits.position < t0_cfi.position
        val normalHit = hasProvider && btbCondHit && normalPosition
        // The end-CFI update must refer to a real conditional BTB hit; comparing
        // position alone can otherwise train an invalid way (position defaults 0).
        val endCfiHit = hasProvider && btbCondHit && t0_cfi.valid &&
          t0_cfi.attribute.isConditional && t0_cfi.position === btb.bits.position

        val normalNeedUpdateCtr = normalHit && t0_tageMeta.provider_ctr(idx) =/= 0.U
        val normalIncCtr = !normalNeedUpdateCtr

        val endCfiNeedUpdateCtr =  endCfiHit && 
        !(t0_tageMeta.provider_ctr(idx) === 7.U && t0_cfi.taken || 
        t0_tageMeta.provider_ctr(idx) === 0.U && !t0_cfi.taken)
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
          t0_tageMeta.provider_u(idx) =/= 3.U
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
    val t0_providerUpdateNeed =
      VecInit(t0_providerUpdateInfo.map(_._1))

    val t0_providerUpdateInc =
      VecInit(t0_providerUpdateInfo.map(_._2))

    val t0_providerUpdateUseful =
      VecInit(t0_providerUpdateInfo.map(_._3))

    val t0_hitBtbVec =
      t0_providerUpdateInfo.map(_._4)

    val t0_hitPosition =
      VecInit(t0_providerUpdateInfo.map(_._5))
    for (i <- 0 until tageNTables) {
      val writeEntryEn = t0_valid && (t0_providerUpdateNeed(i) || (t0_needAllocate && t0_allocateTableIdx === i.U))
      val alloc = (t0_needAllocate && t0_canAllocate && t0_allocateTableIdx === i.U)
      val hitBtb = t0_hitBtbVec(i).reduce(_||_)
      val hitTageCtr= Mux1H(t0_hitBtbVec(i), t0_tageMeta.provider_ctr)
      val hitTageU= Mux1H(t0_hitBtbVec(i), t0_tageMeta.provider_u)
      val hitPostion = t0_hitPosition(i)
      val hitNewCtr = Mux(t0_providerUpdateNeed(i), inc_ctr(hitTageCtr, t0_providerUpdateInc(i)), hitTageCtr)
      val hitNewU = inc_u(hitTageU, t0_providerUpdateUseful(i))

      tables(i).io.writeReq.valid := t0_valid && (t0_providerUpdateNeed(i) || alloc) 
      tables(i).io.writeReq.bits.setIdx       := t0_hashIdx(i)
      tables(i).io.writeReq.bits.writeEntry   := writeEntryEn
      tables(i).io.writeReq.bits.writeUseful  := t0_providerUpdateUseful(i)
      tables(i).io.writeReq.bits.entry.valid  := writeEntryEn
      tables(i).io.writeReq.bits.entry.tag    := t0_hashTag(i) ^ Mux(alloc,t0_cfi.position, hitPostion)
      tables(i).io.writeReq.bits.entry.ctr    := Mux(alloc, Cat(t0_cfi.taken,0.U(2.W)),hitNewCtr)
      tables(i).io.writeReq.bits.u      := Mux(alloc, 0.U(2.W), hitNewU)
    }

    XSPerfAccumulate("tage_train", t0_valid)
    XSPerfAccumulate("tage_train_cfi", t0_valid && t0_cfi.valid)
    XSPerfAccumulate("tage_train_cond", t0_valid && t0_cfi.valid && t0_cfi.attribute.isConditional)
    XSPerfAccumulate("tage_train_mispred", t0_valid && t0_cfi.valid && t0_cfi.mispredict)
    XSPerfAccumulate("tage_provider_hit", PopCount(t0_tageMeta.provider.map(_.valid)))
    XSPerfAccumulate("tage_provider_longest", t0_providerIsLongest)
    XSPerfAccumulate("tage_allocate_req", t0_needAllocate)
    XSPerfAccumulate("tage_allocate_do", t0_needAllocate && t0_canAllocate)
    XSPerfAccumulate("tage_provider_ctr_update", PopCount(t0_providerUpdateNeed))
    XSPerfAccumulate("tage_provider_u_update", PopCount(t0_providerUpdateUseful))
    XSPerfAccumulate("tage_provider_hit_entries", PopCount(t0_hitBtbVec.flatten))
    XSPerfAccumulate("tage_u_candidate_provider", PopCount(t0_providerUpdateInfo.flatMap(_._6)))
    XSPerfAccumulate("tage_u_candidate_btb_cond", PopCount(t0_providerUpdateInfo.flatMap(_._7)))
    XSPerfAccumulate("tage_u_candidate_position", PopCount(t0_providerUpdateInfo.flatMap(_._8)))
    XSPerfAccumulate("tage_u_candidate_alt_differs", PopCount(t0_providerUpdateInfo.flatMap(_._9)))
    XSPerfAccumulate("tage_u_candidate_correct", PopCount(t0_providerUpdateInfo.flatMap(_._10)))
    XSPerfAccumulate("tage_u_candidate_not_saturated", PopCount(t0_providerUpdateInfo.flatMap(_._11)))
}
