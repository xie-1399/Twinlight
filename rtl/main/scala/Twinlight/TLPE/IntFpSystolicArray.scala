package Twinlight.TLPE

import Twinlight.TLFpu._
import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class IntFpSystolicArray(require_bias: Boolean, weightBufferSize: Int, PEWidth: Int, weightWidth: Int, actExpWidth: Int, actMantissaWidth: Int, psumExpWidth: Int, psumMantissaWidth: Int, is_wallace: Boolean = false, is_bitslice: Boolean = false) extends TLModule {
  assert(actExpWidth == psumExpWidth)
  assert(actMantissaWidth == psumMantissaWidth) // assume the same precision

  val actFullWidth = actExpWidth + actMantissaWidth + 1
  val psumFullWidth = psumExpWidth + psumMantissaWidth + 1
  // weight --> preload
  //                 0 1 2 3
  // act --> flow  0 * * * *

  val io = new Bundle {
    val rm = in port RoundingEncoding()

    val weights_preload = in port Bool()
    val weights = in port Vec.fill(weightBufferSize)(SInt(weightWidth bits))

    val activations = in port UInt(actFullWidth bits)

    val psums = in port (require_bias generate Vec.fill(PEWidth)(UInt(psumFullWidth bits)))

    val calc_valid = in port Bool()

    val result = out port UInt(psumFullWidth bits)
  }

  val systolicArray = Array.tabulate(PEWidth) { _ =>
    PE_top(require_bias, weightBufferSize, weightWidth, actExpWidth, actMantissaWidth, psumExpWidth, psumMantissaWidth, is_wallace, is_bitslice)
  }

  // preload weights
  // load inputs & connect horizontally
  systolicArray(0).io.rm := io.rm
  systolicArray(0).io.in_a := io.weights
  systolicArray(0).io.a_preload := io.weights_preload
  systolicArray(0).io.calc_valid := io.calc_valid
  systolicArray(0).io.in_b := io.activations
  systolicArray(0).io.in_c := 0 // to arrays' tail
  // systolocArray(0).io.in_d := 0 // ignore

  (1 until PEWidth).foreach { x =>
    val pe_last = systolicArray(x - 1)
    val pe = systolicArray(x)
    pe.io.rm := pe_last.io.out_rm
    pe.io.in_a := pe_last.io.out_a
    pe.io.a_preload := pe_last.io.out_preload
    pe.io.calc_valid := pe_last.io.out_calc_valid
    pe.io.in_b := pe_last.io.out_b
    pe.io.in_c := pe_last.io.out_c
    // systolocArray(0).io.in_d := 0 // ignore
  }

  io.result := systolicArray(PEWidth - 1).io.out_c

}