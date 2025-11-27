package Twinlight.TLFpu

import spinal.core._

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