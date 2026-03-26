package Twinlight.TLPE

import Twinlight.TLFpu._
import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class PE_top(require_bias: Boolean, weightBufferSize: Int, weightWidth: Int, actExpWidth: Int, actMantissaWidth: Int, psumExpWidth: Int, psumMantissaWidth: Int, is_wallace: Boolean = false, is_bitslice: Boolean = false) extends TLModule {
  assert(actExpWidth == psumExpWidth)
  assert(actMantissaWidth == psumMantissaWidth) // assume the same precision

  val actFloatintWidth = actExpWidth + actMantissaWidth + 1
  val psumFloatintWidth = psumExpWidth + psumMantissaWidth + 1
  val io = new Bundle {
    val rm = in port RoundingEncoding()
    val out_rm = out port RoundingEncoding()

    val in_a = in port Vec.fill(weightBufferSize)(SInt(weightWidth bits))
    val a_preload = in port Bool()
    val out_a = out port Vec.fill(weightBufferSize)(SInt(weightWidth bits))
    val out_preload = out port Reg(Bool())

    val calc_valid = in port Bool()
    val out_calc_valid = out port Bool()
    val in_b = in port UInt(actFloatintWidth bits)
    val in_c = in port UInt(psumFloatintWidth bits)
    val in_d = in port (require_bias generate UInt(psumFloatintWidth bits))

    val out_b = out port Reg(UInt(actFloatintWidth bits))
    val out_c = out port Reg(UInt(psumFloatintWidth bits))
  }


  val weight_r = Vec.fill(weightBufferSize)(Reg(SInt(weightWidth bits)))
  io.out_a := weight_r
  io.out_preload := io.a_preload
  io.out_rm := io.rm
  io.out_calc_valid := io.calc_valid

  when(io.a_preload) {
    weight_r := io.in_a
  }

  val weight_sel_cnt = Reg(UInt(log2Up(weightBufferSize) bits)) init 0
  when((weight_sel_cnt === weightBufferSize - 1) || !io.calc_valid) {
    weight_sel_cnt := 0
  } otherwise {
    weight_sel_cnt := weight_sel_cnt + 1
  }

  val multiplier = FMULFused(expWidth = actExpWidth, precision = actMantissaWidth, intWidth = weightWidth, is_wallace = is_wallace, is_bitslice = is_bitslice)

  multiplier.io.a := io.in_b
  multiplier.io.b := weight_r(weight_sel_cnt)
  multiplier.io.rm := io.rm
  val product = multiplier.io.result

  // TODO: FP precision conversion
  val adder = FPU_ADD(expWidth = psumExpWidth, precision = psumMantissaWidth)

  // TODO: depends on the latency of FP adder
  val accum_valid_r = Reg(Bool()) init False
  accum_valid_r := io.calc_valid

  adder.io.a := multiplier.io.result
  when(accum_valid_r) {
    adder.io.b := io.out_c
  } otherwise {
    if (require_bias) {
      adder.io.b := io.in_d
    } else {
      adder.io.b := U(0).resized
    }
  }
  adder.io.rm := io.rm

  //  io.out_a := io.in_a // bypass weight
  io.out_b := io.in_b // bypass activation
  //  io.out_c := product
  io.out_c := adder.io.result

}