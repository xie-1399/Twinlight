package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import Twinlight.TLUtils.BitSliceUnit
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class BitSliceMultiplier(multiplicandWidth: Int, multiplierWidth: Int, sliceWidth: Int) extends TLModule {
  val io = new Bundle {
    val multiplicand = in port SInt(multiplicandWidth bits)
    val multiplier = in port SInt(multiplierWidth bits)

    val product = out port SInt(multiplicandWidth + multiplierWidth bits)
  }

  val multiplicand_slices = BitSliceUnit(multiplicandWidth, sliceWidth, io.multiplicand.resize(multiplicandWidth + sliceWidth bits).asBits)
  val partial_products = Vec.tabulate(multiplicandWidth / sliceWidth + 1) { i =>
    ((multiplicand_slices(i) * io.multiplier) << (2 * i)).resize(multiplicandWidth + multiplierWidth bits)
  }

  io.product := partial_products.reduceBalancedTree(_ + _)
}