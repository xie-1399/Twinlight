package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class RoundModes() {
  val RNE: UInt = U(0, 3 bits)
  val RTZ: UInt = U(1, 3 bits)
  val RDN: UInt = U(2, 3 bits)
  val RUP: UInt = U(3, 3 bits)
  val RMM: UInt = U(4, 3 bits)
}

class RoundingUnit(val width: Int) extends TLModule {
  val io = new Bundle {
    val inx = in port UInt(width bits)
    val roundIn = in port Bool()
    val stickyIn = in port Bool()
    val signIn = in port Bool()
    val rm = in port UInt(3 bits)
    val outx = out port UInt(width bits)
    val inexact = out port Bool()
    val cout = out port Bool()
    val r_up = out port Bool()
  }

  val (g, r, s) = (io.inx.lsb, io.roundIn, io.stickyIn)
  val inexact = r | s
  val r_up = Bool()
  switch(io.rm) {
    is(RoundModes().RNE) {
      r_up := ((r && s) || (r && !s && g))
    }
    is(RoundModes().RTZ) {
      r_up := False
    }
    is(RoundModes().RUP) {
      r_up := (inexact & !io.signIn)
    }
    is(RoundModes().RDN) {
      r_up := (inexact & io.signIn)
    }
    is(RoundModes().RMM) {
      r_up := r
    }
    default {
      r_up := False
    }
  }

  val out_r_up = io.inx + U(1, io.inx.getWidth bits)
  io.outx := Mux(r_up, out_r_up, io.inx)
  io.inexact := inexact
  // r_up && io.in === 111...1
  io.cout := r_up && io.inx.andR
  io.r_up := r_up
}

object RoundingUnit {
  def apply(inx: UInt, rm: UInt, sign: Bool, width: Int): RoundingUnit = {
    require(inx.getWidth >= width)
    val in_pad = if (inx.getWidth < width + 2) padd_tail(inx, width + 2) else inx
    val rounder = new RoundingUnit(width)

    rounder.io.inx := in_pad.asBits.resizeLeft(width).asUInt
    rounder.io.roundIn := in_pad(in_pad.getWidth - (width + 1))
    rounder.io.stickyIn := in_pad.trim(width + 1).orR
    rounder.io.rm := rm
    rounder.io.signIn := sign
    rounder
  }

  def padd_tail(x: UInt, w: Int): UInt = Cat(x, U(0, (w - x.getWidth) bits)).asUInt

  def is_rmin(rm: UInt, sign: Bool): Bool = {
    rm === RoundModes().RTZ || (rm === RoundModes().RDN && !sign) || (rm === RoundModes().RUP && sign)
  }
}

class TininessRounder(expWidth: Int, precision: Int) extends TLModule {

  val io = new Bundle() {
    val inx = in port RawFloat(expWidth, precision + 3)
    val rm = in port UInt(3 bits)
    val tininess = out port Bool()
  }

  val rounder = RoundingUnit(
    io.inx.mantissa(io.inx.mantissa.getWidth - 3 downto 0).asUInt,
    io.rm,
    io.inx.sign,
    precision - 1
  )

  val tininess = (io.inx.mantissa(io.inx.mantissa.getWidth - 1 downto io.inx.mantissa.getWidth - 2) === B(0, 2 bits)) ||
    ((io.inx.mantissa(io.inx.mantissa.getWidth - 1 downto io.inx.mantissa.getWidth - 2) === B(1, 2 bits)) && !rounder.io.cout)

  io.tininess := tininess
}

object TininessRounder {
  def apply(expWidth: Int, precision: Int, in: RawFloat, rm: UInt): Bool = {
    val tininess_rounder = new TininessRounder(expWidth, precision)
    tininess_rounder.io.inx := in
    tininess_rounder.io.rm := rm
    tininess_rounder.io.tininess
  }
}