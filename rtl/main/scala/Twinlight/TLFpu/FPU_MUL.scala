package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps


case class FMULToFADD_fflags() extends Bundle with IMasterSlave {
  val isNaN = Bool()
  val isInf = Bool()
  val isInv = Bool()
  val overflow = Bool()

  def asMaster(): Unit = {
    out(isNaN, isInf, isInv, overflow)
  }
}

case class FMULToFADD(expWidth: Int, precision: Int) extends Bundle with IMasterSlave {
  val fp_prod = Floating(expWidth, 2 * precision)
  val inter_flags = FMULToFADD_fflags()
  val rm = RoundingEncoding()

  def asMaster(): Unit = {
    out(fp_prod, rm)
    master(inter_flags)
  }
}

case class FPU_MUL(expWidth: Int, precision: Int) extends TLModule {
  val manWidthWithHiddenOne = precision + 1
  val io = new Bundle {
    val interface = master(FPU_IF(expWidth, precision))
    val to_fadd = master(FMULToFADD(expWidth, precision))
  }

  val raw_a = RawFloat.fromUInt(io.interface.a, expWidth, precision)
  val raw_b = RawFloat.fromUInt(io.interface.b, expWidth, precision)


}

case class FMUL_special_info() extends Bundle {
  val nan = Bool()
  val inf = Bool()
  val inv = Bool()
  val hasZero = Bool()
}

case class FMUL_s1_to_s2(expWidth: Int, precision: Int) extends Bundle with IMasterSlave {
  val special_case = Flow(FMUL_special_info())
  val early_overflow = Bool()
  val prod_sign = Bool()
  val shift_amt = UInt(expWidth + 1 bits)
  val exp_shifted = UInt(expWidth + 1 bits)
  val may_be_subnormal = Bool()
  val rm = RoundingEncoding()

  override def asMaster(): Unit = {
    out(early_overflow, prod_sign, shift_amt, exp_shifted, may_be_subnormal, rm, special_case)
  }
}

case class FMUL_s2_to_s3(expWidth: Int, precision: Int) extends Bundle with IMasterSlave {
  val paddingBits = precision + 2
  val special_case = Flow(FMUL_special_info())
  val raw_out = RawFloat(expWidth + 1, paddingBits + 2 * precision)
  val early_overflow = Bool()
  val rm = RoundingEncoding()

  override def asMaster(): Unit = {
    out(special_case, raw_out, early_overflow, rm)
  }
}

case class FMUL_s1(val expWidth: Int, val precision: Int) extends TLModule {
  val io = new Bundle() {
    val a, b = in port UInt(expWidth + precision bits)
    val rm = in port RoundingEncoding()
    val out = master(FMUL_s1_to_s2(expWidth, precision))
  }

  val fp_a = Floating.fromUInt(io.a, expWidth, precision)
  val fp_b = Floating.fromUInt(io.b, expWidth, precision)
  val (decode_a, decode_b) = (fp_a.decode, fp_b.decode)
  val raw_a = RawFloat.fromFP(fp_a, Some(decode_a.expNotZero))
  val raw_b = RawFloat.fromFP(fp_b, Some(decode_b.expNotZero))

  val prod_sign = fp_a.sign ^ fp_b.sign

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
  val paddingBits = precision + 2
  val padding = U(0, paddingBits bits)
  val biasInt = Floating.expBias(expWidth)
  require(biasInt > paddingBits)
  val exp_sum = raw_a.exponent.asUInt.expand + raw_b.exponent.asUInt.expand
  val prod_exp = exp_sum - U(biasInt - (paddingBits + 1))

  val shift_lim_sub = U"1'b0" @@ exp_sum - U(biasInt - paddingBits)
  val prod_exp_uf = shift_lim_sub.msb
  val shift_lim = shift_lim_sub.trim(1)
  // ov <=> exp_a + exp_b - bias > max_exp
  val prod_exp_ov = exp_sum >
    U(Floating.maxNormExp(expWidth) + Floating.expBias(expWidth))

  val subnormal_sig = Mux(decode_a.expIsZero, raw_a.mantissa, raw_b.mantissa)
  val lzc = CLZ(Cat(padding, subnormal_sig).asUInt)
  val exceed_lim = shift_lim <= lzc
  val shift_amt = Mux(prod_exp_uf, U(0), Mux(exceed_lim, shift_lim, lzc))

  val exp_shifted = prod_exp - shift_amt

  io.out.early_overflow := prod_exp_ov
  io.out.prod_sign := prod_sign
  io.out.shift_amt := shift_amt
  io.out.exp_shifted := exp_shifted
  io.out.may_be_subnormal := exceed_lim || prod_exp_uf
  io.out.rm := io.rm

  /*
      Special cases
   */
  val hasZero = decode_a.isZero || decode_b.isZero
  val hasNaN = decode_a.isNaN || decode_b.isNaN
  val hasSNaN = decode_a.isSNaN || decode_b.isSNaN
  val hasInf = decode_a.isInf || decode_b.isInf
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

case class FMUL_s2(val expWidth: Int, val precision: Int) extends TLModule {
  val paddingBits = precision + 2
  val io = new Bundle() {
    val inx = slave(FMUL_s1_to_s2(expWidth, precision))
    val prod = in port UInt(2 * precision bits)
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
  val sig_shifted_raw = (sig_shifter_in << shift_amt)(paddingBits + 2 * precision - 1 downto 0)
  val exp_is_subnormal = io.inx.may_be_subnormal && !sig_shifted_raw.msb
  val no_extra_shift = sig_shifted_raw.msb || exp_is_subnormal

  val exp_pre_round = Mux(exp_is_subnormal, U(0), Mux(no_extra_shift, exp_shifted, exp_shifted - U(1)))
  val sig_shifted = Mux(no_extra_shift, sig_shifted_raw, Cat(sig_shifted_raw.asUInt.trim(1), U"1'b0"))

  io.out.raw_out.sign := prod_sign
  io.out.raw_out.exponent := exp_pre_round.asBits
  io.out.raw_out.mantissa := sig_shifted

}

case class FMUL_s3(val expWidth: Int, val precision: Int) extends TLModule {
  val paddingBits = precision + 2
  val io = new Bundle() {
    val inx = slave(FMUL_s2_to_s3(expWidth, precision))
    val result = out port UInt(expWidth + precision bits)
    val fflags = out port UInt(5 bits)
    val to_fadd = master(FMULToFADD(expWidth, precision))
  }

  val rm = io.inx.rm
  val prod_sign = io.inx.raw_out.sign

  val exp_pre_round = io.inx.raw_out.exponent
  val sig_shifted = io.inx.raw_out.mantissa

  val raw_in = new RawFloat(expWidth, precision + 3)
  raw_in.sign := prod_sign
  raw_in.exponent := exp_pre_round
  raw_in.mantissa := Cat(sig_shifted.asBits.resizeLeft(precision + 2), sig_shifted.asUInt.trim(precision + 2).orR)

  val tininess = TininessRounder(expWidth, precision, raw_in, rm)

  val rounder = RoundingUnit(
    raw_in.mantissa.asUInt.trim(1), // hidden bit is not needed
    rm,
    raw_in.sign,
    precision - 1
  )

  val exp_rounded = rounder.io.cout.asUInt + raw_in.exponent.asUInt
  val sig_rounded = rounder.io.outx

  val common_of = Mux(
    rounder.io.cout,
    raw_in.exponent === B((BigInt(1) << expWidth) - 2, expWidth bits),
    raw_in.exponent === B((BigInt(1) << expWidth) - 1, expWidth bits)
  ) || io.inx.early_overflow
  val common_ix = rounder.io.inexact | common_of
  val common_uf = tininess & common_ix

  val rmin = RoundingUnit.is_rmin(rm, raw_in.sign)

  val of_exp = Mux(rmin,
    U((BigInt(1) << expWidth) - 2, expWidth bits),
    U((BigInt(1) << expWidth) - 1, expWidth bits)
  )
  val common_exp = Mux(
    common_of,
    of_exp,
    exp_rounded(expWidth - 1 downto 0)
  )
  val common_sig = Mux(
    common_of,
    Mux(rmin, (U"1'b1" #* (precision - 1)).asUInt, U(0).resized),
    sig_rounded
  )
  val common_result =
    Cat(raw_in.sign, common_exp, common_sig)

  val common_fflags = Cat(False, False, common_of, common_uf, common_ix)

  val special_case = io.inx.special_case
  val special_result = Mux(special_case.payload.nan,
    Floating.defaultNaNUInt(expWidth, precision), // default NaN
    Mux(special_case.payload.inf,
      Cat(
        raw_in.sign,
        U((BigInt(1) << expWidth) - 1, expWidth bits),
        U(0, precision - 1 bits)).asUInt, // inf
      Cat(raw_in.sign, U(0, expWidth + precision - 1 bits)).asUInt // zero
    )
  )
  val special_fflags = Cat(special_case.payload.inv, False, False, False, False)

  io.result := Mux(special_case.valid, special_result, common_result.asUInt)
  io.fflags := Mux(special_case.valid, special_fflags.asUInt, common_fflags.asUInt)

  io.to_fadd.rm := io.inx.rm
  io.to_fadd.fp_prod.sign := prod_sign
  io.to_fadd.fp_prod.exponent := Mux(special_case.payload.hasZero, B(0), exp_pre_round)
  io.to_fadd.fp_prod.mantissa := Mux(special_case.payload.hasZero,
    B(0),
    sig_shifted.asUInt.trim(1).asBits.resizeLeft(2 * precision - 1) | (sig_shifted.asUInt.trim(2 * precision).orR #* (2 * precision - 1))
  )
  io.to_fadd.inter_flags.isInv := special_case.payload.inv
  io.to_fadd.inter_flags.isInf := special_case.payload.inf && !special_case.payload.nan
  io.to_fadd.inter_flags.isNaN := special_case.payload.nan
  io.to_fadd.inter_flags.overflow := exp_pre_round.asUInt > (U"1'b1" #* expWidth).asUInt
}

case class FMUL(expWidth: Int, precision: Int) extends TLModule {
  val io = new Bundle() {
    val a, b = in port UInt(expWidth + precision bits)
    val rm = in port RoundingEncoding()
    val result = out port UInt(expWidth + precision bits)
    val fflags = out port UInt(5 bits)
    val to_fadd = master(FMULToFADD(expWidth, precision))
  }

  val multiplier = Multiplier(precision + 1 + 1, pipeAt = Seq())
  val fmul_s1 = FMUL_s1(expWidth, precision)
  val fmul_s2 = FMUL_s2(expWidth, precision)
  val fmul_s3 = FMUL_s3(expWidth, precision)


  val raw_a = RawFloat.fromUInt(io.a, expWidth, precision)
  val raw_b = RawFloat.fromUInt(io.b, expWidth, precision)

  multiplier.io.a := raw_a.mantissa.asUInt.expand // multiplier requires a sign bit.
  multiplier.io.b := raw_b.mantissa.asUInt.expand
  multiplier.io.regEnables.foreach(_ := True)

  fmul_s1.io.a := io.a
  fmul_s1.io.b := io.b
  fmul_s1.io.rm := io.rm

  fmul_s2.io.inx := fmul_s1.io.out
  fmul_s2.io.prod := multiplier.io.result

  fmul_s3.io.inx := fmul_s2.io.out

  io.to_fadd := fmul_s3.io.to_fadd
  io.result := fmul_s3.io.result
  io.fflags := fmul_s3.io.fflags

}