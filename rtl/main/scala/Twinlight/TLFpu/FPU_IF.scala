package Twinlight.TLFpu

import spinal.core._
import spinal.lib.IMasterSlave

import scala.language.postfixOps

case class FPU_IF(expWidth: Int, precision: Int) extends Bundle with IMasterSlave {
  // 1 for the sign bit
  val inputWidth = expWidth + precision + 1

  val a, b = UInt(inputWidth bits)
  val rm = RoundingEncoding() // round mode
  val result = UInt(inputWidth bits)
  val fflags = UInt(5 bits)

  def asMaster(): Unit = {
    in(a, b, rm)
    out(result, fflags)
  }
}
