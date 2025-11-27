package Twinlight.TLFpu

import spinal.core._

import scala.language.postfixOps

// it must be a case class, or the cloneOf method cannot retrieve the construction param of such a Bundle
case class RawFloat(exponentSize: Int,
                    mantissaSize: Int) extends Bundle {
  // here, the mantissa field CONTAINS the implicit first bit
  // hence the width of a RawFloat is 33 bits, not 32 bits
  val sign = Bool()
  val exponent = Bits(exponentSize bits)
  val mantissa = Bits(mantissaSize bits)
}

/* let the floating to the RawFloat*/
object RawFloat {
  def fromFP(fp: Floating, expNotZero: Option[Bool] = None): RawFloat = {
    val inner = RawFloat(fp.exponentSize, fp.mantissaSize + 1)
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