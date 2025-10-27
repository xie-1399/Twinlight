package TLFpu

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/**
 *  follow the Apache-2.0 License.(c), All rights reserved *
 * Floating point value
 * updated from https://github.com/SpinalHDL/SpinalHDL/blob/dev/lib/src/main/scala/spinal/lib/experimental/math/Floating.scala
 * @param exponentSize Size of the exponent field
 * @param mantissaSize Size of the mantissa field with the implicit one not included
 */

object RoundingEncoding extends SpinalEnum{
  val RNE, RTZ, RDN, RUP, RMM = newElement()
  defaultEncoding = SpinalEnumEncoding("RoundingEncoding")(
    RNE -> 0,
    RTZ -> 1,
    RDN -> 2,
    RUP -> 3,
    RMM -> 4
  )
}

case class FPBundle() extends Bundle{
  val expNotZero = Bool()
  val expIsZero = Bool()
  val expIsOnes = Bool()
  val manNotZero = Bool()
  val manIsZero = Bool()
  val isSubnormal = Bool()
  val isInf = Bool()
  val isZero = Bool()
  val isNaN = Bool()
  val isSNaN = Bool()
  val isQNaN = Bool()
}

case class Floating(exponentSize: Int,
                    mantissaSize: Int) extends Bundle {

  /** Mantissa field without the implicit first bit */
  val mantissa = Bits(mantissaSize bits)

  /** Exponent field (127 excess encoded) */
  val exponent = Bits(exponentSize bits)

  /** Sign field (true when negative) */
  val sign = Bool()

  /** Value of the exponent bias for this float configuration */
  def getExponentBias = ((1 << (exponentSize - 1)) - 1)

  /** Return true if the number is positive */
  def isPositive = !sign

  def decode:FPBundle = {
    val expNotZero = exponent.orR
    val expIsOnes = exponent.andR
    val manNotZero = mantissa.orR
    val bundle = new FPBundle
    bundle.expNotZero := expNotZero
    bundle.expIsZero := !expNotZero
    bundle.expIsOnes := expIsOnes
    bundle.manNotZero := manNotZero
    bundle.manIsZero := !manNotZero
    bundle.isSubnormal := bundle.expIsZero && manNotZero
    bundle.isInf := bundle.expIsOnes && bundle.manIsZero
    bundle.isZero := bundle.expIsZero && bundle.manIsZero
    bundle.isNaN := bundle.expIsOnes && bundle.manNotZero
    bundle.isSNaN := bundle.isNaN && !mantissa.msb
    bundle.isQNaN := bundle.isNaN && mantissa.msb
    bundle
  }
  /**
   * Assign decimal value to the Floating
   * @param that BigDecimal value
   */
  def := (that: BigDecimal) = {
    // Handle zero case
    if (that == 0) {
      this.exponent := B(0)
      this.mantissa := B(0)
      this.sign := False

    } else {

      val inputValue = that.abs

      var shiftAmount = 0
      var shiftedMantissa = inputValue
      while ((shiftedMantissa.toBigInt & (BigInt(1) << mantissaSize)) == 0) {
        shiftedMantissa *= 2
        shiftAmount += 1
      }

      def firstBitIndex = mantissaSize - shiftAmount
      this.mantissa := B(shiftedMantissa.toBigInt).resized
      this.exponent := (getExponentBias + firstBitIndex)
      this.sign := Bool(that < 0)
    }
  }

  /**
   * Initialization function
   * @param that initialization value
   * @return returns initialized object
   */
  def init(that: BigDecimal): this.type = {
    val initValue = cloneOf(this)
    initValue := that
    this init (initValue)
    this
  }
  /**
   * Absolute value
   * @return Absolute value of this float
   */
  def abs: Floating = FloatingAbs(this)
}


object Floating {
  def expBias(expWidth: Int): BigInt = {
    (BigInt(1) << (expWidth - 1)) - 1
  }

  def maxNormExp(expWidth: Int): BigInt = {
    (BigInt(1) << expWidth) - 2
  }

  /* Uint to Float Point */
  def fromUInt(x: UInt, expWidth: Int, manWidth: Int): Floating = {
    val fp = new Floating(expWidth, manWidth)
    fp.sign := x(expWidth + manWidth)
    fp.exponent := x(expWidth + manWidth - 1 downto manWidth).asBits
    fp.mantissa := x(manWidth - 1 downto 0).asBits
    fp
  }

  /* Default Q-NAN */
  def defaultNaNUInt(expWidth: Int, manWidth: Int): UInt = {
    (B(0, 1 bits) ## Repeat(B"1", expWidth + 1) ## Repeat(B"0", manWidth - 1)).asUInt
  }

  def defaultNaN(expWidth: Int, manWidth: Int): Floating = {
    fromUInt(defaultNaNUInt(expWidth, manWidth), expWidth, manWidth)
  }
}

class RawFloat(exponentSize: Int,
               mantissaSize: Int) extends Bundle {
  val sign = Bool()
  val exponent = Bits(exponentSize bits)
  val mantissa = Bits(mantissaSize bits)
}

/* let the floating to the RawFloat*/
object RawFloat {
  def fromFP(fp: Floating, expNotZero: Option[Bool] = None): RawFloat = {
    val inner = new RawFloat(fp.exponentSize, fp.mantissaSize)
    val nz = if (expNotZero.isDefined) expNotZero.get else fp.exponent.orR
    inner.sign := fp.sign
    inner.exponent := Mux(nz, fp.exponent, B(1, fp.exponentSize bits))
    inner.mantissa := nz ## fp.mantissa
    inner
  }

  def fromUInt(x: UInt, expWidth: Int, manWidth: Int): RawFloat = {
    val fp = Floating.fromUInt(x, expWidth, manWidth)
    val raw = fromFP(fp)
    raw
  }
}

/** Half precision IEEE 754 */

/* FP4 */
object Floating4_E2M1{
  def apply() = Floating(2, 1)
}

object Floating4_E3M0{
  def apply() = Floating(3, 0)
}

/* FP6 */
object Floating6_E3M2{
  def apply() = Floating(3, 2)
}

object Floating6_E2M3{
  def apply() = Floating(2, 3)
}

/* FP8 */
object Floating8_E4M3{
  def apply() = Floating(4, 3)
}

object Floating8_E5M2{
  def apply() = Floating(5, 2)
}

/* FP16 */
object Floating16 {
  def apply() = Floating(5, 10)
}

/** Single precision IEEE 754 */
object Floating32 {
  def apply() = Floating(8, 23)
}

/** Double precision IEEE 754 */
object Floating64 {
  def apply() = Floating(11, 52)
}

/** Quad precision IEEE 754 */
object Floating128 {
  def apply() = Floating(15, 112)
}
