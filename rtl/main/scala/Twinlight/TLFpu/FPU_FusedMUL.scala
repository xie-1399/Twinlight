package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class Lb_ceil(intWidth: Int) extends TLModule {
  val depth = log2Up(intWidth)
  val io = new Bundle() {
    val a = in port UInt(intWidth bits)
    val o = out port UInt(depth bits)
  }

  //  val indicators = Vec.tabulate(depth){ d =>
  //    val width = 1 << d
  //    Vec.tabulate((intWidth/width).ceil.toInt){ w =>
  //      val s = w * width
  //      io.a((s + width - 1).min(intWidth) downto s).asBools.reduceBalancedTree(_ | _)
  //    }
  //  }

  // the most simple version for a functional test
  val (sFound, sIdx) = io.a.asBools.reverse.sFindFirst(_ === True)
  io.o := sFound ? (U(intWidth - 1).resized - sIdx) | sIdx.getZero
}

object Lb_ceil {
  def apply(x: UInt): UInt = {
    val lbceil = Lb_ceil(x.getWidth)
    lbceil.io.a := x
    lbceil.io.o
  }
}

case class FInt(width: Int, expWidth: Int, manWidth: Int) extends Bundle with IMasterSlave {
  val sign = Bool()
  val absolute = UInt(width bits)
  val exponent = UInt(expWidth bits)
  val mantissa = UInt(manWidth bits)

  val isZero = Bool()

  override def asMaster(): Unit = {
    out(sign, absolute, exponent, mantissa, isZero)
  }
}

object FInt {
  def fromSInt(x: SInt, expWidth: Int, manWidth: Int): FInt = {
    assert(x.getWidth <= manWidth)
    val fi = FInt(x.getWidth, expWidth, manWidth)
    fi.sign := x.msb
    fi.absolute := (fi.sign ? (~x + 1) | x).asUInt.resized
    fi.exponent := Lb_ceil(fi.absolute).resized // the exponent part of int b (w/o bias)
    fi.mantissa := ((fi.absolute @@ U(0, manWidth - x.getWidth bits)) << (7 - fi.exponent)).resize(manWidth bits)
    fi.isZero := x === 0
    fi
  }
}

case class FMULFused_s1(expWidth: Int, precision: Int, intWidth: Int) extends TLModule {
  val manWidthWithHiddenOne = precision + 1
  val fullWidth = expWidth + precision + 1
  val io = new Bundle() {
    val a = in port UInt(fullWidth bits)
    val b = in port SInt(intWidth bits)
    val rm = in port RoundingEncoding()
    val out = master(FMUL_s1_to_s2(expWidth, precision))
    val fi_b = master(FInt(intWidth, expWidth, manWidthWithHiddenOne))
  }

  val fp_a = Floating.fromUInt(io.a, expWidth, precision)
  val fi_b = FInt.fromSInt(io.b, expWidth, manWidthWithHiddenOne)

  io.fi_b.sign := fi_b.sign
  io.fi_b.exponent := fi_b.exponent
  io.fi_b.mantissa := fi_b.mantissa
  io.fi_b.absolute := fi_b.absolute
  io.fi_b.isZero := fi_b.isZero

  val decode_a = fp_a.decode
  val raw_a = RawFloat.fromFP(fp_a, Some(decode_a.expNotZero))
  val raw_b = fi_b // just a copy

  val prod_sign = fp_a.sign ^ fi_b.sign

  /*
      prod = xx.xxx...xxx
      sig_pre_shift = precision | g | s | prod
      padding = precision | g | s
      paddingBits = precision + 2
      if prod <- [2, 4):
        prod_exp = a.exp + b.exp - bias + paddingBits + 1
      if prod <- [1, 2):
        prod_exp = a.exp + b.exp - bias + paddingBits
      we assume product <- [2, 4) at first
   */
  val paddingBits = manWidthWithHiddenOne + 2
  val padding = U(0, paddingBits bits)
  val biasInt = Floating.expBias(expWidth)
  require(biasInt > paddingBits)
  val exp_sum = raw_a.exponent.asUInt.expand + raw_b.exponent.resized
  val prod_exp = exp_sum + U(paddingBits + 1) // no extra -bias required, we assume product <- [2, 4) at first

  val shift_lim = exp_sum + U(paddingBits) // it will not underflow anyway
  // ov <=> exp_a + exp_b > max_exp
  val prod_exp_ov = exp_sum > U(Floating.maxNormExp(expWidth)) // it might overflow

  // b is a subnormal number iff. b is zero
  val subnormal_sig = raw_a.mantissa
  val lzc = CLZ(Cat(padding, subnormal_sig).asUInt)
  val exceed_lim = shift_lim <= lzc
  val shift_amt = Mux(exceed_lim, shift_lim, lzc)

  val exp_shifted = prod_exp - shift_amt

  io.out.early_overflow := prod_exp_ov
  io.out.prod_sign := prod_sign
  io.out.shift_amt := shift_amt
  io.out.exp_shifted := exp_shifted
  io.out.may_be_subnormal := exceed_lim
  io.out.rm := io.rm

  /*
      Special cases
   */
  val hasZero = decode_a.isZero || fi_b.isZero
  val hasNaN = decode_a.isNaN
  val hasSNaN = decode_a.isSNaN
  val hasInf = decode_a.isInf
  val special_case_happen = hasZero || hasNaN || hasInf

  val zero_mul_inf = hasZero && hasInf
  val nan_result = hasNaN || zero_mul_inf
  val special_iv = hasSNaN || zero_mul_inf

  io.out.special_case.valid := special_case_happen
  io.out.special_case.payload.nan := nan_result
  io.out.special_case.payload.inf := hasInf
  io.out.special_case.payload.inv := special_iv
  io.out.special_case.payload.hasZero := hasZero

}

case class FMULFused_s2(expWidth: Int, precision: Int) extends TLModule {
  val paddingBits = precision + 3
  val manWidthWithHiddenOne = precision + 1
  val io = new Bundle() {
    val inx = slave(FMUL_s1_to_s2(expWidth, precision))
    val prod = in port UInt(2 * manWidthWithHiddenOne bits)
    val out = master(FMUL_s2_to_s3(expWidth, precision))
  }

  io.out.special_case := io.inx.special_case
  io.out.early_overflow := io.inx.early_overflow
  io.out.rm := io.inx.rm

  /*
    prod = xx.xxx...xxx
    sig_pre_shift = precision | g | s | prod
    padding = precision | g | s
    paddingBits = precision + 2
    if prod <- [2, 4):
      prod_exp = a.exp + b.exp - bias + paddingBits + 1
    if prod <- [1, 2):
      prod_exp = a.exp + b.exp - bias + paddingBits
    we assume product <- [2, 4) at first
 */

  val padding = U(0, paddingBits bits)

  val rm = io.inx.rm
  val prod = io.prod
  val prod_sign = io.inx.prod_sign
  val shift_amt = io.inx.shift_amt
  val exp_shifted = io.inx.exp_shifted

  val sig_shifter_in = Cat(padding, prod)
  val sig_shifted_raw = (sig_shifter_in << shift_amt).resize(paddingBits + 2 * precision + 2)
  val exp_is_subnormal = io.inx.may_be_subnormal && !sig_shifted_raw.msb
  val no_extra_shift = sig_shifted_raw.msb || exp_is_subnormal

  val exp_pre_round = Mux(exp_is_subnormal, U(0), Mux(no_extra_shift, exp_shifted, exp_shifted - U(1)))
  val sig_shifted = Mux(no_extra_shift, sig_shifted_raw, sig_shifted_raw.asUInt.trim(1) ## U"1'b0")


  io.out.raw_out.sign := prod_sign
  io.out.raw_out.exponent := exp_pre_round.asBits
  io.out.raw_out.mantissa := sig_shifted

}

case class FMULFused(expWidth: Int, precision: Int, intWidth: Int, is_wallace: Boolean = false, is_bitslice: Boolean = false) extends TLModule {
  val manWidthWithHiddenOne = precision + 1
  val fullWidth = expWidth + precision + 1
  val io = new Bundle() {
    // float
    val a = in port UInt(fullWidth bits)
    // integer
    val b = in port SInt(intWidth bits)
    val rm = in port RoundingEncoding()
    val result = out port UInt(fullWidth bits)
    val fflags = out port UInt(5 bits)
    val to_fadd = master(FMULToFADD(expWidth, precision))
  }

  val fmul_s1 = FMULFused_s1(expWidth, precision, intWidth)
  val fmul_s2 = FMULFused_s2(expWidth, precision)
  val fmul_s3 = FMUL_s3(expWidth, precision)


  val raw_a = RawFloat.fromUInt(io.a, expWidth, precision)
  val raw_b = fmul_s1.io.fi_b

  fmul_s1.io.a := io.a
  fmul_s1.io.b := io.b
  fmul_s1.io.rm := io.rm

  if (is_bitslice) {
    val multiplier = BitSliceMultiplier(manWidthWithHiddenOne + 1, manWidthWithHiddenOne + 1, 2, wallaceTree = is_wallace)
    multiplier.io.multiplier := raw_a.mantissa.asUInt.expand.asSInt
    multiplier.io.multiplicand := raw_b.mantissa.expand.asSInt

    fmul_s2.io.prod := multiplier.io.product.trim(2).asUInt
  } else if (is_wallace) {
    // 011 * 011 = extra sign | sign | 1001
    // sign | hidden bit | precision
    val multiplier = Multiplier(manWidthWithHiddenOne + 1, pipeAt = Seq())
    multiplier.io.a := raw_a.mantissa.asUInt.expand // multiplier requires a sign bit.
    multiplier.io.b := raw_b.mantissa.expand
    multiplier.io.regEnables.foreach(_ := True)

    fmul_s2.io.prod := multiplier.io.result.trim(2)

  } else {
    fmul_s2.io.prod := (raw_a.mantissa.asUInt.expand * raw_b.mantissa.expand).trim(2)
  }

  fmul_s2.io.inx := fmul_s1.io.out
  fmul_s3.io.inx := fmul_s2.io.out

  io.to_fadd := fmul_s3.io.to_fadd
  io.result := fmul_s3.io.result
  io.fflags := fmul_s3.io.fflags

}