//******************************************************************************
// Copyright (c) 2017 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.fdip.ifu.bpd.ras

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._


import boom.fdip.common._
import boom.fdip.exu.{CommitExceptionSignals, BranchDecode, BrUpdateInfo}
import boom.fdip.util._
import boom.fdip.ifu._
import boom.fdip.ifu.bpd.RasUpdateInfo

/** Translate prediction, FTQ recovery and retirement into stack operations. */
class BoomRAS(implicit p: Parameters) extends RasModule()(p) {
  val io = IO(new Bundle {
    val specIn = Flipped(Valid(new RasSpecInfo))
    val redirect = Flipped(Valid(new RasUpdateInfo))
    val commit = Flipped(Valid(new RasCommitInfo))
    val topRetAddr = Output(UInt(vaddrBitsExtended.W))
    val meta = Output(new RasMeta)
    val specNearOverflow = Output(Bool())
  })

  private val stack = Module(new BoomRasStack)
  private val spec = io.specIn.bits
  stack.io.spec.fire := useRAS.B && io.specIn.valid && !io.redirect.valid
  stack.io.spec.pushValid := stack.io.spec.fire && spec.attribute.hasPush && !stack.io.specNearOverflow
  stack.io.spec.popValid := stack.io.spec.fire && spec.attribute.hasPop && !stack.io.specNearOverflow
  // cfiPosition denotes the terminal halfword, for both RVC and RVI CFIs.
  stack.io.spec.pushAddr := fetchAlign(spec.startPc) + (spec.cfiPosition << 1.U) + 2.U

  private val redirect = io.redirect.bits
  stack.io.redirect.valid := useRAS.B && io.redirect.valid
  stack.io.redirect.isCall := redirect.cfiValid && redirect.taken && redirect.attribute.hasPush
  stack.io.redirect.isRet := redirect.cfiValid && redirect.taken && redirect.attribute.hasPop
  stack.io.redirect.callAddr := fetchAlign(redirect.startPc) + (redirect.cfiPosition << 1.U) + 2.U
  stack.io.redirect.meta.ssp := redirect.rasMeta.ssp
  stack.io.redirect.meta.sctr := redirect.rasMeta.sctr
  stack.io.redirect.meta.tosr := redirect.rasMeta.tosr
  stack.io.redirect.meta.tosw := redirect.rasMeta.tosw
  stack.io.redirect.meta.nos := redirect.rasMeta.nos

  private val commit = io.commit.bits
  stack.io.commit.valid := useRAS.B && io.commit.valid
  stack.io.commit.pushValid := commit.cfiValid && commit.taken && commit.attribute.hasPush
  stack.io.commit.popValid := commit.cfiValid && commit.taken && commit.attribute.hasPop
  stack.io.commit.pushAddr := fetchAlign(commit.startPc) + (commit.cfiPosition << 1.U) + 2.U
  stack.io.commit.metaSsp := commit.meta.ssp
  stack.io.commit.metaTosw := commit.meta.tosw

  io.topRetAddr := stack.io.spec.popAddr
  io.meta.ssp := stack.io.meta.ssp
  io.meta.sctr := stack.io.meta.sctr
  io.meta.tosr := stack.io.meta.tosr
  io.meta.tosw := stack.io.meta.tosw
  io.meta.nos := stack.io.meta.nos
  io.meta.topRetAddr := stack.io.spec.popAddr
  io.specNearOverflow := stack.io.specNearOverflow
}
