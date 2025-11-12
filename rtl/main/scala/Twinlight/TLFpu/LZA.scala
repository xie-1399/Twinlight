package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class LZA(len: Int) extends TLModule {
  val io = new Bundle {
    val a = in port UInt(len bits)
    val b = in port UInt(len bits)
    val f = out port UInt(len bits)
  }

  val p = (io.a ^ io.b).asBools
  val g = (io.a & io.b).asBools
  val k = (~io.a & ~io.b).asBools // also annihilate

  io.f := Vec.tabulate(len)({ i =>
    if (i == 0){
      False
    }else{
      p(i) ^ !k(i-1) //TODO Why are the generation signals missing? however it works. I shall figure it out later.
    }
  }).asBits.asUInt
}

object LZA {
  def apply(len: Int, a: UInt, b: UInt) : UInt = {
    val lza = new LZA(len)
    lza.io.a := a
    lza.io.b := b
    lza.io.f
  }
}