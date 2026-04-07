package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import Twinlight.TLUtils.BitSliceUnit
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class BitSliceMultiplier(multiplicandWidth: Int, multiplierWidth: Int, sliceWidth: Int, wallaceTree: Boolean = true, pipeAt: Seq[Int] = Seq()) extends TLModule {
  val io = new Bundle {
    val multiplicand = in port SInt(multiplicandWidth bits)
    val multiplier = in port SInt(multiplierWidth bits)

    val product = out port SInt(multiplicandWidth + multiplierWidth bits)
  }

  val multiplicand_slices = BitSliceUnit(multiplicandWidth, sliceWidth, io.multiplicand.resize(multiplicandWidth + sliceWidth bits).asBits)
  val columns: Array[Seq[Bool]] = Array.fill(multiplicandWidth + multiplierWidth)(Seq())

  val partial_products = Vec.tabulate(multiplicandWidth / sliceWidth + 1) { i =>
    val pp_temp = multiplicand_slices(i) * io.multiplier
    if (wallaceTree) {
      val s = pp_temp.msb
      val pp = if (i == 0) Cat(~s, s, s, pp_temp) else Cat(U"1", ~s, pp_temp)
      for (j <- 2 * i until 2 * i + pp.getWidth) {
        if (j < columns.length) {
          columns(j) = columns(j) :+ pp(j - 2 * i)
        }
      }
    }
    (pp_temp << (2 * i)).resize(multiplicandWidth + multiplierWidth bits)
  }

  if (wallaceTree) {
    // wallace tree version
    val (sum, carry) = Multiplier(multiplicandWidth, pipeAt).addAll(cols = columns, depth = 0)
    io.product := (sum + carry).asSInt
  } else {
    // reduce tree version
    // add latency manually
    val prod_r = Vec.fill(pipeAt.size)(Reg(SInt(multiplicandWidth + multiplierWidth bits)))
    prod_r(0) := partial_products.reduceBalancedTree(_ + _)
    (1 until prod_r.size).foreach { x => prod_r(x) := prod_r(x - 1) }
    io.product := prod_r.last
  }


}