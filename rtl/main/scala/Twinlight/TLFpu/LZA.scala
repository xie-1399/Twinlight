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

  val p = Vec.fill(len)(Bool())
  val k = Vec.fill(len)(Bool())

  io.f := Vec.tabulate(len)({ i =>
    p(i) := io.a(i) ^ io.b(i)
    k(i) := (!io.a(i)) & (!io.b(i))
    if (i == 0){
      False
    }else{
      p(i) ^ (!k(i-1))
    }
  }).asBits.reversed.asUInt
}

object LZA {
  def apply(len: Int, a: UInt, b: UInt) : UInt = {
    val lza = new LZA(len)
    lza.io.a := a
    lza.io.b := b
    lza.io.f
  }
}