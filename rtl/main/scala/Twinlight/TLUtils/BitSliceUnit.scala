package Twinlight.TLUtils

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class BitSliceUnit(dataWidth: Int, sliceWidth: Int) extends TLModule {
  val inputWidth = dataWidth + sliceWidth
  val sliceNum = dataWidth / sliceWidth + 1
  val io = new Bundle {
    val data = in port Bits(inputWidth bits)
    val slices = out port Vec.fill(sliceNum)(SInt(sliceWidth bits))
  }

  val carry_vec = Vec.fill(sliceNum + 1)(Bool())
  carry_vec(0) := False

  io.slices.zipWithIndex.foreach { case (i, idx) =>
    val input_value = io.data(idx * sliceWidth, sliceWidth bits).resize(sliceWidth + 1).asSInt
    val slice_value = input_value + carry_vec(idx).asBits((sliceWidth + 1) bits).asSInt
    carry_vec(idx + 1) := (slice_value.asBits(sliceWidth - 1) | slice_value.asBits(sliceWidth))
    i := slice_value.asBits.resize(sliceWidth).asSInt
  }

}

object BitSliceUnit{
  def apply(dataWidth: Int, sliceWidth: Int, inputData: Bits) : Vec[SInt] = {
    val bsu = BitSliceUnit(dataWidth, sliceWidth)
    bsu.io.data := inputData
    bsu.io.slices
  }
}
