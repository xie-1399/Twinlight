package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class LZA(len: Int) extends TLModule {
  // One must ensure that the MSB of both io.a and io.b is zero.
  val io = new Bundle {
    val a = in port UInt(len bits)
    val b = in port UInt(len bits)
    val f = out port UInt(len bits)
  }

  val p = (io.a ^ io.b)
  val k = (~io.a & ~io.b) // also annihilate

  io.f := Vec.tabulate(len)({ i =>
    if (i == 0) {
      False
    } else {
      p(i) ^ !k(i - 1) //TODO Why are the generation signals missing? however it works. I shall figure it out later.
    }
  }).asBits.asUInt
}

object LZA {
  def apply(a: UInt, b: UInt): UInt = {
    assert(a.getWidth == b.getWidth, "The widths of LZA inputs are not equal to each other.")
    val lza = LZA(a.getWidth)
    lza.io.a := a
    lza.io.b := b
    lza.io.f
  }
}