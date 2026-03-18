package Twinlight.TLPE

import Twinlight.TLFpu._
import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class PE_top(weightWidth: Int, actExpWidth: Int, actMantissaWidth: Int, psumExpWidth: Int, psumMantissaWidth: Int, is_wallace: Boolean = false, is_bitslice: Boolean = false) extends TLModule {
  assert(actExpWidth == psumExpWidth)
  assert(actMantissaWidth == psumMantissaWidth) // assume the same precision

  val actFloatintWidth = actExpWidth + actMantissaWidth + 1
  val psumFloatintWidth = psumExpWidth + psumMantissaWidth + 1
  val io = new Bundle {
    val rm = in port RoundingEncoding()

    val in_a = in port SInt(weightWidth bits)
    val a_preload = in port Bool()
    val in_b = in port UInt(actFloatintWidth bits)
    val in_d = in port UInt(psumFloatintWidth bits)

    val out_a = out port SInt(weightWidth bits)
    val out_b = out port UInt(actFloatintWidth bits)
    val out_c = out port UInt(psumFloatintWidth bits)
  }

  val weight_r = Reg(SInt(weightWidth bits))

  when(io.a_preload){
    weight_r := io.in_a
  }

  val multiplier = FMULFused(expWidth = actExpWidth, precision = actMantissaWidth, intWidth = weightWidth, is_wallace = is_wallace, is_bitslice = is_bitslice)

  multiplier.io.a := io.in_b
  multiplier.io.b := weight_r
  multiplier.io.rm := io.rm
  val product = multiplier.io.result

  // TODO: FP precision conversion
  val adder = FPU_ADD(expWidth = psumExpWidth, precision = psumMantissaWidth)

  adder.io.a := multiplier.io.result
  adder.io.b := io.in_d
  adder.io.rm := io.rm

  io.out_a := io.in_a // bypass weight
  io.out_b := io.in_b // bypass activation
//  io.out_c := product
  io.out_c := adder.io.result

}