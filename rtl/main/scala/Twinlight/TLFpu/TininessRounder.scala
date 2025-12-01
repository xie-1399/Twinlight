package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class TininessRounder(expWidth: Int, precision: Int) extends TLModule {

  val io = new Bundle() {
    val inx = in port RawFloat(expWidth, precision + 3)
    val rm = in port RoundingEncoding()
    val tininess = out port Bool()
  }

  val rounder = RoundingUnit(
    io.inx.mantissa.asUInt.trim(2),
    io.rm,
    io.inx.sign,
    precision - 1
  )

  val tininess = (io.inx.mantissa.resizeLeft(2) === B(0).resized) || ((io.inx.mantissa.resizeLeft(2) === B(1).resized) && !rounder.io.cout)

  io.tininess := tininess
}

object TininessRounder {
  def apply(expWidth: Int, precision: Int, in: RawFloat, rm: RoundingEncoding.C): Bool = {
    val tininess_rounder = new TininessRounder(expWidth, precision)
    tininess_rounder.io.inx := in
    tininess_rounder.io.rm := rm
    tininess_rounder.io.tininess
  }
}