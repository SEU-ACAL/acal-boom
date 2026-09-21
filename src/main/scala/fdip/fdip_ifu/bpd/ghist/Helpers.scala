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

/** Pure combinational helpers shared by the FDIP PHR implementation. */
trait PhrHelpers extends HasPhrParameters {
  /** Rotate a fixed-width value left by a compile-time amount. */
  def circularShiftLeft(src: UInt, amount: Int): UInt = {
    require(src.getWidth > 0, "cannot rotate a zero-width value")
    require(amount >= 0 && amount < src.getWidth,
      s"rotation amount $amount must be in [0, ${src.getWidth - 1}]")
    val doubled = Cat(src, src)
    doubled(src.getWidth * 2 - 1 - amount, src.getWidth - amount)
  }

  /**
    * XOR bit/value contributions into selected positions of a folded history.
    * Empty positions are legal and are driven to zero.
    */
  def bitsetsXor(width: Int, bitsets: Seq[Seq[(Int, Bool)]]): UInt = {
    require(width > 0, "folded history width must be positive")
    val result = Wire(Vec(width, Bool()))
    for (i <- 0 until width) {
      val bits = bitsets.flatMap(_.collect { case (pos, bit) if pos == i => bit })
      result(i) := (if (bits.isEmpty) false.B else bits.reduce(_ ^ _))
    }
    result.asUInt
  }

  /**
    * Hash the CFI PC and target. The low PHRShamt bits are appended to the
    * circular history; the remaining bits are folded into predictor history.
    */
  def pathHash(cfiPc: UInt, target: UInt): UInt = {
    require(PHRHashWidth >= PHRShamt, "PHR hash width must cover the shift amount")
    val pcBits = (cfiPc >> 1) ^ (cfiPc >> 3) ^ (cfiPc >> 5) ^ (cfiPc >> 7)
    val targetBits = target >> 2
    (pcBits ^ targetBits)(PHRHashWidth - 1, 0)
  }

  def getPathHashComponents(cfiPc: UInt, target: UInt): (UInt, UInt) = {
    val hash = pathHash(cfiPc, target)
    (hash(PHRShamt - 1, 0), hash(PHRHashWidth - 1, PHRShamt))
  }

  /**
    * Compute the XOR-fold of the low `historyLength` bits into `foldedWidth`
    * bits. This is the reference implementation used before incremental
    * folded-history maintenance is added.
    */
  def computeFoldedHist(history: UInt, historyLength: Int, foldedWidth: Int): UInt = {
    require(historyLength > 0, "history length must be positive")
    require(foldedWidth > 0, "folded history width must be positive")
    require(history.getWidth >= historyLength,
      s"history width ${history.getWidth} is smaller than $historyLength")
    bitsetsXor(foldedWidth,
      Seq((0 until historyLength).map(i => (i % foldedWidth, history(i)))))
  }

  /** Fold a path hash into a predictor field of arbitrary width. */
  def computeFoldedHash(hash: UInt, foldedWidth: Int): UInt = {
    require(foldedWidth > 0, "folded hash width must be positive")
    val hashWidth = hash.getWidth
    require(hashWidth > 0, "path hash must have a known width")
    bitsetsXor(foldedWidth,
      Seq((0 until hashWidth).map(i => (i % foldedWidth, hash(i)))))
  }

  /**
    * Restore the PHR state belonging to a prediction and apply its final
    * taken-CFI result. Not-taken updates deliberately leave the snapshot
    * unchanged so an override can remove an earlier speculative insertion.
    */
  def getUpdatePtrs(data: PhrUpdate, hashHigh: UInt): PhrUpdateResult = {
    require(hashHigh.getWidth == PHRHashHighWidth,
      s"hash high width ${hashHigh.getWidth} does not match $PHRHashHighWidth")
    val result = WireInit(0.U.asTypeOf(new PhrUpdateResult))
    result.ptr := data.phrMeta.ptr
    result.phrLowBits := data.phrMeta.phrLowBits
    when (data.valid && data.prediction.taken) {
      result.ptr := data.phrMeta.ptr.dec(PHRShamt)
      result.phrLowBits := data.phrMeta.phrLowBits ^ hashHigh
    }
    result
  }
}
