package Twinlight.TLFpu

import spinal.core._

/**
 * Returns absolute value of the floating point number
 */
object FloatingAbs {
  /**
   * Returns the absolute value of an IEEE754 float
   * @param that input value
   * @return outputs absolute value of the input number
   */
  def apply(that: Floating): Floating = {
    val x = cloneOf(that)
    x.mantissa := that.mantissa
    x.exponent := that.exponent
    x.sign := False
    x
  }
}
