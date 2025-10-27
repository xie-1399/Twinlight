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


/**
 * Floating comparison result
 */
case class FloatingCompareResult() extends Bundle {
  /** a less than b */
  val lessThan = Bool()

  /** a less or equal to b */
  val lessThanEqual = Bool()

  /** a equals b */
  val equals = Bool()

  /** a greater than b */
  val greaterThan = Bool()

  /** a greater or equal to b */
  val greaterThanEqual = Bool()

  /** both nan or one argument is signalling NaN */
  val invalid = Bool()
}