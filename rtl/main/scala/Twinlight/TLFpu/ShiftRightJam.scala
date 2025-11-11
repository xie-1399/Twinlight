package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class ShiftRightJam(len: Int, expWidth: Int) extends TLModule {
  val max_shamt = log2Up(len + 1)
  val io = new Bundle {
    val i = in port Bits(len bits)
    val shamt = in port UInt(expWidth bits)
    val o = out port Bits(len bits)
    val sticky = out port Bool()
  }

  val exceed = io.shamt > len
  val shamt = io.shamt.resize(max_shamt bits)
  val stickyMask = ((U(1) << shamt) - 1).resize(len bits).asBits | B(exceed) #* len

  io.o := Mux(exceed, B(0, len bits), (io.i >> shamt).resize(len bits))
  io.sticky := (io.i.asBits & stickyMask).orR
}

object ShiftRightJam {
  def apply(i: Bits, shamt: UInt): (Bits, Bool) = {
    val shifter = new ShiftRightJam(i.getWidth, shamt.getWidth)
    shifter.io.i := i
    shifter.io.shamt := shamt
    (shifter.io.o, shifter.io.sticky)
  }
}