package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

// This is the main function that generates the VHDL and the Verilog corresponding to MyTopLevel.
object VerilogGenerator {
  def main(args: Array[String]) {
    SpinalConfig(
      mode=Verilog,
      targetDirectory="rtl/gen/FMUL_wallace"
    ).generate(FMUL(expWidth = 5, precision = 10, is_traditional = false))

    SpinalConfig(
      mode=Verilog,
      targetDirectory="rtl/gen/FMUL_traditional"
    ).generate(FMUL(expWidth = 5, precision = 10, is_traditional = true))
  }
}

