package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class CLZ(len: Int) extends TLModule{
  val inWidth = len
  val outWidth = U(inWidth - 1).getWidth

  val io = new Bundle{
    val clz_in = in port UInt(inWidth bits)
    val clz_out = out port UInt(outWidth bits)
  }

  io.clz_out := io.clz_in.asBits.reversed.asBools.sFindFirst({i => i===True})._2
}

object CLZ {
  def apply(value: UInt): UInt = {
    val clz = new CLZ(value.getWidth)
    clz.io.clz_in := value
    clz.io.clz_out
  }
}