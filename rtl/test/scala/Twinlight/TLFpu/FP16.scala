package Twinlight.TLFpu

class FP16 {

  def getSign32(x: Int): Int = {
    (x >> 31) & 0x1
  }

  def getExp32(x: Int): Int = {
    (x >> 23) & 0xFF
  }

  def getSig32(x: Int): Int = {
    0x7fffff & x
  }

  def getSign16(x: Int): Int = {
    (x >> 15) & 0x1
  }

  def getExp16(x: Int): Int = {
    (x >> 10) & 0x1F
  }

  def getSig16(x: Int): Int = {
    0x3ff & x
  }

  def unpack32(x: Float): (Int, Int, Int) = {
    val xint = java.lang.Float.floatToRawIntBits(x)
    (getSign32(xint), getExp32(xint), getSig32(xint))
  }

  def pack32(sign: Int, exp: Int, sig: Int): Float = {
    val xint = sign << 31 | exp << 23 | sig
    java.lang.Float.intBitsToFloat(xint)
  }

  def unpack16(x: Float): (Int, Int, Int) = {
    val xint = java.lang.Float.floatToRawIntBits(x)
    f32to16(getSign32(xint), getExp32(xint), getSig32(xint))
  }

  def pack16(sign: Int, exp: Int, sig: Int): Float = {
    val exp1 = if (exp == 0x1f) 0xff else exp - 15 + 127
    var ix = 13
    val exp2 = if ((exp == 0) && (sig != 0)) {
      while (((((sig << ix) >> 23) & 0x1) == 0)) {
        ix = ix + 1
      }
      exp1 - (ix - 13) + 1
    } else if ((exp == 0) && (sig == 0)) {
      0
    } else {
      exp1
    }
    val xint = sign << 31 | exp2 << 23 | (sig << ix) & 0x7fffff
    //    println(s"${sign}, ${exp2.toHexString}, ${sig.toHexString} = ${xint.toHexString}")
    java.lang.Float.intBitsToFloat(xint)
  }

  def f32to16(sign: Int, exp: Int, sig: Int): (Int, Int, Int) = {
    val g = ((sig >> 12) & 0x01) == 1
    val r = ((sig >> 11) & 0x01) == 1
    val s = (sig & 0x7ff) == 1

    val exp1 = exp - 127 + 15
    val sig1 = ((sig >> 13) & 0x3ff) + (if ((r && s) || (r && !s && g)) 1 else 0)

    //    println(sign, exp, sig)
    //    println(sign, exp1, sig1)

    val sig2 = if (exp1 == 0) {
      sig1 >> 1 | (1 << 9)
    } else if (exp1 < 0) {
      (sig1 >> 1 | (1 << 9)) >> exp1.abs
    } else {
      sig1
    }

    val exp_tmp = if (exp == 0) 0 else if (exp == 0xff) 0x1f else if (exp1 > 30) 30 else if (exp1 == 0) 0 else if (exp1 < 0) 0 else exp1
    val sig_tmp = if (exp == 0) sig1 else if (exp == 0xff) sig1 else if (exp1 > 30) 0x3ff else if (exp1 <= 0) sig2 else sig1
    val expf = if (((sig_tmp >> 10) & 0x1) == 1) exp_tmp + 1 else exp_tmp
    val sigf = sig_tmp & 0x3ff
    (sign, expf, sigf)
  }

  def float2int16(x: Float): Int = {
    //    println(s"f2i16: ${x}")
    val (sign32, exp32, sig32) = unpack32(x)
    //    println(s"exp = ${exp32}, sig = ${sig32}")
    // fp16: 5 bits exponent, 10 bits mantissa
    val (sign16, exp16, sig16) = f32to16(sign32, exp32, sig32)
    //    println(s"exp = ${exp16}, sig = ${sig16}")
    (sign16 << 15 | exp16 << 10 | sig16) & 0xFFFF
  }

  def int16tofloat(x: Int): Float = {
    if (getExp16(x) == 0x1F && getSig16(x) == 0) {
      //Inf
      if (getSign16(x) == 1) Float.NegativeInfinity else Float.PositiveInfinity
    } else if (getExp16(x) == 0x1F && getSig16(x) != 0) {
      // NaN
      Float.NaN
    } else if (getExp16(x) == 0) {
      // Subnormal
      pack16(getSign16(x), getExp16(x), getSig16(x))
    } else {
      pack16(getSign16(x), getExp16(x), getSig16(x))
    }
  }

}
