package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class RoundingUnit(width: Int) extends TLModule {
  val io = new Bundle {
    val inx = in port UInt(width bits)
    val roundIn = in port Bool()
    val stickyIn = in port Bool()
    val signIn = in port Bool()
    val rm = in port RoundingEncoding()
    val outx = out port UInt(width bits)
    val inexact = out port Bool()
    val cout = out port Bool()
    val r_up = out port Bool()
  }

  val (g, r, s) = (io.inx.lsb, io.roundIn, io.stickyIn)
  val inexact = r | s
  val r_up = Bool()
  r_up := io.rm.mux(
    RoundingEncoding.RNE -> ((r && s) || (r && !s && g)),
    RoundingEncoding.RTZ -> False,
    RoundingEncoding.RUP -> (inexact & !io.signIn),
    RoundingEncoding.RDN -> (inexact & io.signIn),
    RoundingEncoding.RMM -> r
  )

  val out_r_up = io.inx + U(1).resized
  io.outx := Mux(r_up, out_r_up, io.inx)
  io.inexact := inexact
  // r_up && io.in === 111...1
  io.cout := r_up && io.inx.andR
  io.r_up := r_up
}

object RoundingUnit {
  def apply(inx: UInt, rm: RoundingEncoding.C, sign: Bool, width: Int): RoundingUnit = {
    require(inx.getWidth >= width)
    val in_pad = if (inx.getWidth < width + 2) inx.asBits.resizeLeft(width + 2).asUInt else inx
    val rounder = new RoundingUnit(width)

    // MSB                                    LSB
    // xxxx_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx_xx|xx
    // ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ||
    // mantissa, width bits                  |\
    //                                 roundIn \
    //                                          stickyIn

    rounder.io.inx := in_pad.asBits.resizeLeft(width).asUInt
    rounder.io.roundIn := in_pad(in_pad.getWidth - (width + 1))
    rounder.io.stickyIn := in_pad.trim(width + 1).orR
    rounder.io.rm := rm
    rounder.io.signIn := sign
    rounder
  }

  def is_rmin(rm: RoundingEncoding.C, sign: Bool): Bool = {
    rm === RoundingEncoding.RTZ || (rm === RoundingEncoding.RDN && !sign) || (rm === RoundingEncoding.RUP && sign)
  }
}

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