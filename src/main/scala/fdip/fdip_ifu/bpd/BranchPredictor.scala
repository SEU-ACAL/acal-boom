     1	package boom.fdip.ifu.bpd
     2	
     3	import chisel3._
     4	import chisel3.util._
     5	
     6	import org.chipsalliance.cde.config.{Field, Parameters}
     7	import freechips.rocketchip.diplomacy._
     8	import freechips.rocketchip.tilelink._
     9	
    10	import boom.fdip.common._
    11	import boom.fdip.util._
    12	import boom.perf._
    13	import boom.fdip.ifu._
    14	import boom.fdip.ifu.bpd.ubtb._
    15	import boom.fdip.ifu.bpd.l1btb._
    16	import boom.fdip.ifu.bpd.tage._
    17	import boom.fdip.ifu.bpd.ras._
    18	import java.lang
    19	
    20	
    21	class BranchPredictor(implicit p: Parameters) extends BpdModule()(p)
    22	{
    23	  val io = IO(new Bundle {
    24	
    25	    // Requests and responses
    26	    val fromFtq       = Flipped(new FTQToBPUIO)
    27	    val toFtq         = new BPUToFTQIO
    28	    val reset_vector  = Input(UInt(vaddrBitsExtended.W))
    29	    // val resp = Output(new Bundle {
    30	    //   val f1 = new BranchPredictionBundle
    31	    //   val f2 = new BranchPredictionBundle
    32	    //   val f3 = new BranchPredictionBundle
    33	    // })
    34	
    35	    val f3_fire = Input(Bool())
    36	
    37	    // Update
    38	    // val update = Input(Valid(new BranchPredictionUpdate))
    39	  })
    40	
    41	  //TODO: BTB need to record call or ret so that we can use RAS and predict update
    42	  val ras               = Module(new BoomRAS)
    43	//如果ftq没有空间，那么s1阶段会一直保持这个pc，并且持续预测（或者使用fire发送预测请求？）
    44	/* 
    45	由于送入bpu的是s0阶段的请求，如果s1阶段没有fire，
    46	 */
    47	  val s0_pc             = WireInit(io.reset_vector)
    48	  val s0_valid          = WireInit(false.B)
    49	  val s0_ghist          = WireInit((0.U).asTypeOf(new GlobalHistory))
    50	  val s0_fire, s1_fire, s2_fire, s3_fire = WireInit(false.B)
    51	  //由于s1阶段可能由于ftq full导致预测信息无法写入，此时需要去replay pc和ghist
    52	  val s1_replay          = WireInit(false.B)
    53	  
    54	  val s1_ready, s2_ready, s3_ready = WireInit(false.B)
    55	  dontTouch(s0_valid)
    56	  dontTouch(s0_ghist)
    57	  val s1_pc             = RegEnable(s0_pc,s0_fire)
    58	  val s1_clear          = WireInit(false.B)
    59	  val s1_valid          = RegInit(false.B)
    60	  val s1_ghist          = RegEnable(s0_ghist,s0_fire)
    61	  // val s1_target         = Wire(UInt(vaddrBitsExtended.W))
    62	
    63	  val s2_pc             = RegEnable(s1_pc,s1_fire)
    64	  val s2_valid          = RegInit(false.B)//RegEnable(s1_valid,false.B,io.toFtq.resp.ready&&(!s1_clear))
    65	  val s2_redirect       = WireInit(false.B)
    66	  val s2_clear          = WireInit(false.B)
    67	  val s2_ghist          = RegEnable(s1_ghist,s1_fire)
    68	  // val s2_target         = Wire(UInt(vaddrBitsExtended.W))
    69	  val s2_resp           = WireInit(io.toFtq.resp.bits.f2)
    70	
    71	  val s3_pc             = RegEnable(s2_pc,s2_fire)
    72	  val s3_valid          = RegInit(false.B)//RegEnable(s2_valid,false.B,s2_fire&&(!s2_clear))
    73	  val s3_clear          = WireInit(false.B)
    74	  val s3_redirect       = WireInit(false.B)
    75	  val s3_ghist          = RegEnable(s2_ghist,s2_fire)
    76	  val s3_resp           = WireInit(io.toFtq.resp.bits.f3)
    77	
    78	  // Fixed predictor topology: FAMicroBTB -> BTB -> TAGE.
    79	  val microBtb = Module(new FAMicroBTBBranchPredictor())
    80	  val btb      = Module(new L1BTB())
    81	  val tage     = Module(new TageBranchPredictor())
    82	
    83	  val bpdF0Valid = s0_valid
    84	  val bpdF0Pc    = fetchAlign(s0_pc)
    85	  val bpdF1Ghist = RegNext(s0_ghist.histories)
    86	
    87	  microBtb.io.f0_valid := bpdF0Valid
    88	  microBtb.io.f0_pc    := bpdF0Pc
    89	  // microBtb.io.train.valid := s3_valid && io.f3_fire && s3_resp.valid
    90	  // microBtb.io.train.bits := s3_resp.preds(PriorityEncoder(s3_resp.predTakenVec))
    91	  // microBtb.io.train.bits.startPc := s3_pc
    92	  // microBtb.io.train.bits.branch.target := s3_predTarget
    93	  // microBtb.io.train.bits.branch.cfiPosition := s3_resp.cfi_offset.bits
    94	  // microBtb.io.train.bits.branch.attribute := s3_resp.preds(PriorityEncoder(s3_resp.predTakenVec)).attribute
    95	  // microBtb.io.train.bits.branch.taken := s3_resp.preds(PriorityEncoder(s3_resp.predTakenVec)).taken
    96	  // microBtb.io.train.bits.branch.mispredict := s3_redirect
    97	
    98	
    99	  btb.io.f0_valid := bpdF0Valid
   100	  btb.io.f0_pc    := bpdF0Pc
   101	  btb.io.train    := io.fromFtq.btbTrain
   102	
   103	  tage.io.f0_valid := bpdF0Valid
   104	  tage.io.f0_pc    := bpdF0Pc
   105	  tage.io.f1_ghist := bpdF1Ghist
   106	  tage.io.train    := io.fromFtq.tageCommitTrain
   107	
   108	  btb.io.resp_in      := microBtb.io.resp
   109	  tage.io.resp_in     := btb.io.resp
   110	  val bpdResp = tage.io.resp
   111	
   112	  val bpdMeta = Wire(new BranchPredictionMeta)
   113	  bpdMeta.tage     := tage.io.f3_meta
   114	  bpdMeta.btb      := btb.io.f3_meta
   115	  // bpdMeta.microBtb := microBtb.io.f3_meta
   116	  //实质上，只要s1阶段fire，那么s2，s3必然valid,
   117	
   118	  //TODO:delete handshake signal
   119	  s0_fire               := s0_valid && s1_ready
   120	  s1_fire               := s1_valid && s2_ready && io.toFtq.resp.ready
   121	  s2_fire               := s2_valid && s3_ready
   122	  s3_fire               := s3_valid  
   123	// s1阶段ready的条件：s1阶段fire，或者s1阶段无效，或者s1阶段clear（此时s1阶段数据无效）
   124	  s1_ready              := s1_fire || !s1_valid || s1_clear
   125	  s2_ready              := s2_fire || !s2_valid || s2_clear
   126	  s3_ready              := s3_fire || !s3_valid || s3_clear
   127	  
   128	  //当clear信号来临，s1_valid 不需要被清除，因为此时s0_valid就是redirect信号，其他阶段无所谓
   129	  //s2，s3阶段发生clear，需要清除自身的valid，假如s2发生重定向，实际上就是下一个到s2阶段的就是无效信号
   130	  when(s0_fire){
   131	    s1_valid := true.B
   132	  }.elsewhen(s1_fire){
   133	    s1_valid := false.B
   134	  }
   135	
   136	  when(s1_clear){
   137	    s2_valid := false.B
   138	  }.elsewhen(s1_fire ){
   139	    s2_valid := true.B
   140	  }.elsewhen(s2_fire){
   141	    s2_valid := false.B
   142	  }
   143	
   144	//如果s2阶段发生clear，
   145	  when(s2_clear){
   146	    s3_valid := false.B
   147	  }.elsewhen(s2_fire ){
   148	    s3_valid := true.B
   149	  }.elsewhen(s3_fire){
   150	    s3_valid := false.B
   151	  }
   152	  s1_replay := s1_valid && !(s2_ready && io.toFtq.resp.ready)
   153	  // val s3_target         = Wire(UInt(vaddrBitsExtended.W))
   154	  val s3_align_pc       = fetchAlign(s3_pc)
   155	
   156	
   157	  // val s3_taken_vec = WireInit(VecInit((0 until predWidth) map { i =>
   158	  //   s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
   159	  //   (s3_resp.preds(i).is_jal||s3_resp.preds(i).is_jalr ||s3_resp.preds(i).is_call||s3_resp.preds(i).is_ret ||
   160	  //   (s3_resp.preds(i).is_br && s3_resp.preds(i).taken))
   161	  // }))
   162	  // val s3_taken_is_call = WireInit(VecInit((0 until predWidth) map { i =>
   163	  //   s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
   164	  //   (s3_resp.preds(i).is_call)
   165	  // }))
   166	  // val s3_taken_is_rvi_call = WireInit(VecInit((0 until predWidth) map { i =>
   167	  //   s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
   168	  //   (s3_resp.preds(i).is_call)&&s3_resp.preds(i).is_rvi_call
   169	  // }))
   170	  // val s3_taken_is_ret = WireInit(VecInit((0 until predWidth) map { i =>
   171	  //   s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
   172	  //   (s3_resp.preds(i).is_ret)
   173	  // }))
   174	  
   175	  // val s3_is_br     = WireInit(VecInit((0 until predWidth) map { i =>
   176	  //   s3_resp.valid && fetchMask(s3_pc)(i) && s3_resp.preds(i).predicted_pc.valid &&
   177	  //   (s3_resp.preds(i).is_br)
   178	  // }))
   179	  // // val s3_taken_idx = 
   180	  // val s3_predTarget = Mux(s3_taken_vec.reduce(_||_),
   181	  //   Mux(s3_taken_is_ret(PriorityEncoder(s3_taken_vec)) && useRAS.B,
   182	  //     ras.io.read_addr,
   183	  //     s3_resp.preds(PriorityEncoder(s3_taken_vec)).predicted_pc.bits
   184	  //   ),
   185	  //   nextFetch(s3_pc)
   186	  // )
   187	
   188	  // val s3_predGhist = s3_resp.ghist.update(
   189	  //   s3_is_br.asUInt,
   190	  //   s3_taken_vec.reduce(_||_),
   191	  //   s3_resp.preds(PriorityEncoder(s3_taken_vec)).is_br,
   192	  //   PriorityEncoder(s3_taken_vec),
   193	  //   s3_taken_vec.reduce(_||_),
   194	  //   s3_pc,
   195	  //   s3_taken_is_call(PriorityEncoder(s3_taken_vec)),
   196	  //   s3_taken_is_ret(PriorityEncoder(s3_taken_vec))
   197	  // )
   198	
   199	  
   200	
   201	  val s1_previous_pred_info    = RegNext(io.toFtq.resp.bits.f1)
   202	  val s2_previous_pred_info    = RegNext(io.toFtq.resp.bits.f2)
   203	  val s3_saw_s1_pred_info      = RegNext(s1_previous_pred_info)
   204	  val f2_correct_f1_ghist     = WireInit(s1_previous_pred_info.predGhist =/=  io.toFtq.resp.bits.f2.predGhist&& enableGHistStallRepair.B && s2_valid)
   205	  val f3_correct_f2_ghist     = WireInit(s2_previous_pred_info.predGhist =/=  io.toFtq.resp.bits.f3.predGhist&& enableGHistStallRepair.B && s3_valid)
   206	  val f3_correct_f1_ghist     = WireInit(s3_saw_s1_pred_info.predGhist   =/=  io.toFtq.resp.bits.f3.predGhist&& enableGHistStallRepair.B && s3_valid)
   207	  dontTouch(s1_replay)
   208	  dontTouch(f2_correct_f1_ghist)
   209	  dontTouch(f3_correct_f2_ghist)
   210	  dontTouch(f3_correct_f1_ghist)
   211	  s0_pc                 := MuxCase(io.reset_vector,
   212	  Seq(
   213	    (RegNext(reset.asBool) && !reset.asBool) -> io.reset_vector,
   214	    io.fromFtq.redirect_val                  -> io.fromFtq.redirect_pc,
   215	    (s3_redirect || f3_correct_f2_ghist)     -> s3_predTarget,
   216	    (s2_redirect || f2_correct_f1_ghist)     -> io.toFtq.resp.bits.f2.predTarget,
   217	    s1_replay                                -> s1_pc,
   218	    s1_valid                                 -> io.toFtq.resp.bits.f1.predTarget
   219	  )
   220	  )
   221	  s0_valid              := (RegNext(reset.asBool) && !reset.asBool) || io.fromFtq.redirect_val ||
   222	                          s3_redirect || s2_redirect || s1_valid ||s1_replay ||f3_correct_f2_ghist || f2_correct_f1_ghist
   223	  s0_ghist              := MuxCase((0.U).asTypeOf(new GlobalHistory),
   224	  Seq(
   225	    (RegNext(reset.asBool) && !reset.asBool) -> (0.U).asTypeOf(new GlobalHistory),
   226	    io.fromFtq.redirect_val                  -> io.fromFtq.redirect_ghist,
   227	    (s3_redirect||f3_correct_f2_ghist)       -> s3_predGhist,
   228	    (s2_redirect||f2_correct_f1_ghist)       -> io.toFtq.resp.bits.f2.predGhist,
   229	    s1_replay                                -> s1_ghist,                     
   230	    s1_valid                                 -> io.toFtq.resp.bits.f1.predGhist
   231	  )
   232	  )
   233	  //redirect and clear
   234	  //其实这里还需要去做位置判断
   235	  s2_redirect                 := io.toFtq.resp.bits.f2.valid&&(s1_previous_pred_info.valid)&&(s1_previous_pred_info.predTarget=/=io.toFtq.resp.bits.f2.predTarget)&&s2_valid
   236	  s3_redirect                 := io.toFtq.resp.bits.f3.valid&&(s2_previous_pred_info.valid)&&(s2_previous_pred_info.predTarget=/=io.toFtq.resp.bits.f3.predTarget)&&s3_valid
   237	  
   238	
   239	 
   240	  s1_clear                    := io.fromFtq.redirect_val || (s3_redirect||f3_correct_f2_ghist) || (s2_redirect||f2_correct_f1_ghist) 
   241	  s2_clear                    := io.fromFtq.redirect_val || (s3_redirect||f3_correct_f2_ghist)
   242	  s3_clear                    := io.fromFtq.redirect_val
   243	
   244	  io.toFtq.resp.bits.f3_ras_top     := ras.io.read_addr
   245	  io.toFtq.resp.bits.f1.hasRedirect := false.B
   246	  io.toFtq.resp.bits.f2.hasRedirect := (s2_redirect||f2_correct_f1_ghist) && (! s2_clear)
   247	  io.toFtq.resp.bits.f3.hasRedirect := (s3_redirect||f3_correct_f2_ghist) && (! s3_clear)
   248	  io.toFtq.resp.bits.f3_pred_target := s3_predTarget
   249	
   250	  //TODO:add update for RAS
   251	  ras.io.read_idx                   := s3_resp.ghist.ras_idx
   252	
   253	//predict update
   254	  ras.io.write_valid := s3_valid && io.f3_fire && useRAS.B && s3_taken_is_call(PriorityEncoder(s3_taken_vec))
   255	  ras.io.write_addr  := s3_align_pc + (PriorityEncoder(s3_taken_vec) << 1) + Mux(
   256	    s3_taken_is_rvi_call(PriorityEncoder(s3_taken_vec)), 4.U, 2.U)
   257	  ras.io.write_idx   := WrapInc(s3_resp.ghist.ras_idx, nRasEntries)
   258	  // //这里最主要的意思就是去一个周期2 branch
   259	/* ----------------------------------- Req ---------------------------------- */
   260	  // def predMask(pc: UInt) = {
   261	  //   val base = fetchAlign(pc)
   262	  //   val idx = base.extract(log2Ceil(predWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))//这里要求predWidth小于等于8
   263	  //   val mask = WireInit(0.U(predWidth.W))
   264	  //   mask := ((1 << predWidth)-1).U << idx
   265	  //   mask
   266	  // }
   267	/* ---------------------------------- Resp ---------------------------------- */
   268	
   269	  dontTouch(s1_previous_pred_info)
   270	  io.toFtq.resp.bits.f1.preds    := bpdResp.f1
   271	  io.toFtq.resp.bits.f2.preds    := bpdResp.f2
   272	  io.toFtq.resp.bits.f3.preds    := bpdResp.f3
   273	  io.toFtq.resp.bits.f3.meta     := bpdMeta
   274	
   275	
   276	  io.toFtq.resp.bits.f1.pc := RegNext(s0_pc)
   277	  io.toFtq.resp.bits.f2.pc := RegNext(s1_pc)
   278	  io.toFtq.resp.bits.f3.pc := RegNext(s2_pc)
   279	
   280	  io.toFtq.resp.valid         := s1_fire && (!s1_clear) 
   281	  io.toFtq.resp.bits.f1.valid := s1_fire && (!s1_clear) 
   282	  io.toFtq.resp.bits.f1.ghist := s1_ghist
   283	
   284	  io.toFtq.resp.bits.f1.ftq_idx:= DontCare
   285	  
   286	
   287	  io.toFtq.resp.bits.f2.valid   := s2_fire && (!s2_clear)
   288	  io.toFtq.resp.bits.f2.ftq_idx := RegNext(io.fromFtq.ftq_idx)
   289	  io.toFtq.resp.bits.f2.ghist   := s2_ghist
   290	
   291	  io.toFtq.resp.bits.f3.valid := s3_fire && (!s3_clear)
   292	  io.toFtq.resp.bits.f3.ftq_idx:= RegNext(RegNext(io.fromFtq.ftq_idx))
   293	  io.toFtq.resp.bits.f3.ghist := s3_ghist
   294	
   295	
   296	  // We don't care about meta from the f1 and f2 resps
   297	  // Use the meta from the latest resp
   298	  io.toFtq.resp.bits.f1.meta  := DontCare
   299	  io.toFtq.resp.bits.f2.meta  := DontCare
   300	  io.toFtq.resp.bits.f1.lhist := DontCare
   301	  io.toFtq.resp.bits.f2.lhist := DontCare
   302	  io.toFtq.resp.bits.f3.lhist := DontCare
   303	/* --------------------------------- update --------------------------------- */
   304	  XSPerfAccumulate("f2_correct_f1_ghist",   f2_correct_f1_ghist)
   305	  XSPerfAccumulate("f3_correct_f2_ghist",   f3_correct_f2_ghist)
   306	  XSPerfAccumulate("f3_correct_f1_ghist",   f3_correct_f1_ghist)
   307	
   308	  val commit_ghist        = RegInit(0.U.asTypeOf(new GlobalHistory))
   309	  dontTouch(commit_ghist)
   310	  when (io.fromFtq.bpdupdate.valid && io.fromFtq.bpdupdate.bits.is_commit_update) {
   311	    commit_ghist := commit_ghist.update(
   312	      io.fromFtq.bpdupdate.bits.br_mask,
   313	      io.fromFtq.bpdupdate.bits.cfi_taken,
   314	      io.fromFtq.bpdupdate.bits.cfi_is_br,
   315	      io.fromFtq.bpdupdate.bits.cfi_idx.bits,
   316	      io.fromFtq.bpdupdate.bits.cfi_idx.valid,
   317	      fetchAlign(io.fromFtq.bpdupdate.bits.pc),
   318	      io.fromFtq.bpdupdate.bits.cfi_is_call,
   319	      io.fromFtq.bpdupdate.bits.cfi_is_ret
   320	    )
   321	    // assert(commit_ghist.histories === io.fromFtq.bpdupdate.bits.ghist.histories,
   322	    //   "Branch Predictor Commit Update GHist does not match the expected value!")
   323	  }
   324	}

