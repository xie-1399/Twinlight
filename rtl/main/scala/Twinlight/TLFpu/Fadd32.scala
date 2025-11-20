package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class Fadd_spcial_info extends Bundle {
  val iv = Bool()
  val nan = Bool()
  val inf_sign = Bool()
}


class FarPath(expWidth: Int, precision: Int, outPC: Int) extends TLModule {
  val manWidthWithHiddenOne = precision + 1
  val io = new Bundle {
    val a = in port RawFloat(expWidth, manWidthWithHiddenOne) // to include the hidden one in the mantissa field
    val b = in port RawFloat(expWidth, manWidthWithHiddenOne)
    val exp_diff = in port UInt(expWidth bits)
    val expSub = in port Bool()

    val a_mantissa = out port UInt(manWidthWithHiddenOne bits)
    val b_mantissa = out port UInt(manWidthWithHiddenOne + 4 bits) // manWidth + G, R, S bits + ext bit
    val b_sticky = out port Bool()
    val a_exps = out port Vec.fill(3)(UInt(expWidth bits))
    val result = out port RawFloat(expWidth, outPC + 3)
  }
  val (b_shifted, b_sticky) = ShiftRightJam(io.b.mantissa ## B"2'b0", io.exp_diff)

  val b_mantissa_raw = B"1'b0" ## b_shifted ## b_sticky
  val b_mantissa = Mux(io.expSub, ~b_mantissa_raw, b_mantissa_raw).asUInt + io.expSub.asUInt

  io.a_mantissa := io.a.mantissa.asUInt
  io.b_mantissa := b_mantissa
  io.b_sticky := b_sticky
  io.a_exps(0) := io.a.exponent.asUInt + 1
  io.a_exps(1) := io.a.exponent.asUInt
  io.a_exps(2) := io.a.exponent.asUInt - 1
  io.result.sign := io.a.sign
  io.result.exponent := 0
  io.result.mantissa := 0
}

class NearPath(expWidth: Int, precision: Int, outPC: Int) extends TLModule {
  val manWidthWithHiddenOne = precision + 1
  val io = new Bundle {
    val a = in port RawFloat(expWidth, manWidthWithHiddenOne) // same reason as the FarPath module
    val b = in port RawFloat(expWidth, manWidthWithHiddenOne)
    val need_shift_b = in port Bool()
    val rm = in port UInt(3 bits)

    val result = out port RawFloat(expWidth, outPC + 3)
    val sig_is_zero = out port Bool()
    val a_lt_b = out port Bool()
    val lza_error = out port Bool()
    val int_bit = out port Bool()
    val sig_raw = out port UInt(manWidthWithHiddenOne bits)
    val lzc = out port UInt(log2Up(manWidthWithHiddenOne) bits)
  }

  val a_sig = (io.a.mantissa ## B(0, 1 bits)).asUInt
  val b_sig = ((io.b.mantissa ## U(0, 1 bits)) >> io.need_shift_b.asUInt).asUInt
  val b_neg = (~b_sig.asBits).asUInt

  val a_minus_b = (U(0, 1 bits) ## a_sig).asUInt + (U(1, 1 bits) ## b_neg).asUInt + U(1, 1 + a_sig.getWidth bits)
  val a_lt_b = a_minus_b.lsb
  val sig_raw = a_minus_b.asBits(outPC downto 0).asUInt
  val lza_str = LZA(a_sig, b_neg).resize(manWidthWithHiddenOne)
  val lza_str_zero = !lza_str.orR

  val need_shift_lim = io.a.exponent.asUInt < U(precision + 1, io.a.exponent.getWidth bits)
  val mask_able_k_width = log2Up(precision + 1)
  val shift_lim_mask_raw = ((B(1, 1 bits) ## B(0, precision + 1 bits)) >> io.a.exponent(mask_able_k_width - 1 downto 0).asUInt).asUInt(precision downto 0)
  val shift_lim_mask = Mux(need_shift_lim, shift_lim_mask_raw, U(0, shift_lim_mask_raw.getWidth bits))
  val shift_lim_bit = (shift_lim_mask_raw & sig_raw).orR

  val lzc_str = shift_lim_mask | lza_str
  val lzc = CLZ(lzc_str)

  val int_bit_mask = Vec.tabulate(precision + 1)({ i =>
    if (i == precision) {
      lzc_str(i)
    } else {
      lzc_str(i) & !lzc_str(precision - 1 - i downto 0).orR
    }
  })

  val int_bit_predicted = (int_bit_mask.map({ i => i | lza_str_zero }).asBits & sig_raw.asBits).orR
  val int_bit_rshift_1 = ((int_bit_mask.asBits >> U(1, 1 bits)).asUInt & sig_raw).orR

  val exceed_lim_mask = Vec.tabulate(precision + 1)({ i =>
    if (i == precision) {
      False
    } else {
      lza_str(precision - 1 - i downto 0).orR
    }
  })

  val exceed_lim = need_shift_lim && !(exceed_lim_mask.asBits.asUInt & shift_lim_mask_raw).orR
  val int_bit = Mux(exceed_lim, shift_lim_bit, int_bit_rshift_1 || int_bit_predicted)

  val lza_error = !int_bit_predicted && !exceed_lim

  val near_path_sign = Mux(a_lt_b, io.b.sign, io.a.sign)

  val result = RawFloat(expWidth, outPC + 3)
  result.sign := near_path_sign
  result.exponent := io.a.exponent
  result.mantissa := 0

  io.result := result
  io.sig_is_zero := lza_str_zero && !sig_raw(0)
  io.a_lt_b := a_lt_b
  io.lza_error := lza_error
  io.sig_raw := sig_raw
  io.lzc := lzc
  io.int_bit := int_bit
}

class FCMA_ADD_s1_to_s2(val expWidth: Int, val precision: Int, val outPc: Int)
  extends Bundle with IMasterSlave {
  val manWidthWithHiddenOne = precision + 1
  val rm = UInt(3 bits)
  val far_path_out = RawFloat(expWidth, outPc + 3)
  val near_path_out = RawFloat(expWidth, outPc + 3)
  val special_case = Flow(new Fadd_spcial_info)

  // far path addtitional
  val small_add = Bool()
  val far_path_mul_of = Bool()
  val far_sig_a = UInt(manWidthWithHiddenOne bits)
  val far_sig_b = UInt(manWidthWithHiddenOne + 4 bits)
  val far_sig_b_sticky = Bool()
  val far_exp_a_vec = Vec.fill(3)(UInt(expWidth bits))

  // near path additional
  val near_path_sig_is_zero = Bool()
  val near_path_lza_error = Bool()
  val near_path_int_bit = Bool()
  val near_path_sig_raw = UInt(precision + 1 bits)
  val near_path_lzc = UInt(log2Up(precision + 1) bits)
  val sel_far_path = Bool()

  override def asMaster(): Unit = {
    out(rm, far_path_out, near_path_out, special_case,
      small_add, far_path_mul_of, far_sig_a, far_sig_b, far_sig_b_sticky, far_exp_a_vec,
      near_path_sig_is_zero, near_path_lza_error, near_path_int_bit, near_path_sig_raw, near_path_lzc, sel_far_path)
  }
}


class FCMA_ADD_s1(val expWidth: Int, val precision: Int, val outPc: Int)
  extends TLModule {
  // 1 for the sign bit
  val manWidthWithHiddenOne = precision + 1
  val inputWidth = expWidth + manWidthWithHiddenOne
  val io = new Bundle() {
    val a, b = in port UInt(inputWidth bits)
    val rm = in port UInt(3 bits)
    val outx = master(new FCMA_ADD_s1_to_s2(expWidth, precision, outPc))
  }

  val fp_a = Floating.fromUInt(io.a, expWidth, precision)
  val fp_b = Floating.fromUInt(io.b, expWidth, precision)
  val decode_a = fp_a.decode
  val decode_b = fp_b.decode
  val raw_a = RawFloat.fromFP(fp_a, Some(decode_a.expNotZero))
  val raw_b = RawFloat.fromFP(fp_b, Some(decode_b.expNotZero))
  // raw_float: 1 bit sign, expWidth bits exponent, & manWidth + 1 bits mantissa
  val eff_sub = raw_a.sign ^ raw_b.sign

  val small_add = decode_a.expIsZero && decode_b.expIsZero

  // deal with special cases
  val b_isNaN = decode_b.isNaN
  val b_isSNaN = decode_b.isSNaN
  val b_isInf = decode_b.isInf

  val special_path_hasNaN = decode_a.isNaN || b_isNaN
  val special_path_hasSNaN = decode_a.isSNaN || b_isSNaN
  val special_path_hasInf = decode_a.isInf || b_isInf
  val special_path_inf_iv = decode_a.isInf && b_isInf && eff_sub

  val special_case_happen = special_path_hasNaN || special_path_hasInf
  val special_path_iv = special_path_hasSNaN || special_path_inf_iv

  val exp_diff_a_b = (B(0, 1 bits) ## raw_a.exponent).asUInt - (B(0, 1 bits) ## raw_b.exponent).asUInt
  val exp_diff_b_a = (B(0, 1 bits) ## raw_b.exponent).asUInt - (B(0, 1 bits) ## raw_a.exponent).asUInt

  val need_swap = exp_diff_a_b.msb

  val ea_minus_eb = Mux(need_swap, exp_diff_b_a, exp_diff_a_b).resize(expWidth)
  val sel_far_path = !eff_sub || ea_minus_eb > U(1)

  /*
        Far path
   */

  val far_path_inputs = Seq(
    (
      Mux(!need_swap, raw_a, raw_b),
      Mux(!need_swap, raw_b, raw_a),
      Mux(!need_swap, exp_diff_a_b, exp_diff_b_a).resize(expWidth)
    )
  )

  val far_path_mods = far_path_inputs.map { in =>
    val far_path = new FarPath(expWidth, precision, outPc)
    far_path.io.a := in._1
    far_path.io.b := in._2
    far_path.io.exp_diff := in._3
    far_path.io.expSub := eff_sub
    far_path
  }
  val far_path_res = far_path_mods.head.io.result

  /*
        Near path
   */

  val near_path_exp_neq = raw_a.exponent(1 downto 0) =/= raw_b.exponent(1 downto 0)

  val near_path_inputs = Seq(
    (raw_a, raw_b, near_path_exp_neq),
    (raw_b, raw_a, near_path_exp_neq)
  )
  val near_path_mods = near_path_inputs.map { in =>
    val near_path = new NearPath(expWidth, precision, outPc)
    near_path.io.a := in._1
    near_path.io.b := in._2
    near_path.io.need_shift_b := in._3
    near_path.io.rm := io.rm
    near_path
  }

  val near_path_a_lt_b = near_path_mods.head.io.a_lt_b

  io.outx.rm := io.rm
  io.outx.small_add := small_add
  io.outx.sel_far_path := sel_far_path

  io.outx.far_path_out := far_path_mods.head.io.result
  io.outx.far_path_mul_of := decode_b.expIsOnes && !eff_sub
  io.outx.far_sig_a := far_path_mods.head.io.a_mantissa
  io.outx.far_sig_b := far_path_mods.head.io.b_mantissa
  io.outx.far_sig_b_sticky := far_path_mods.head.io.b_sticky
  io.outx.far_exp_a_vec := far_path_mods.head.io.a_exps

  val sel_out = need_swap || (!near_path_exp_neq && near_path_a_lt_b)

  io.outx.near_path_out := Mux(sel_out, near_path_mods.last.io, near_path_mods.head.io).result
  io.outx.near_path_sig_is_zero := Mux(sel_out, near_path_mods.last.io, near_path_mods.head.io).sig_is_zero
  io.outx.near_path_sig_raw := Mux(sel_out, near_path_mods.last.io, near_path_mods.head.io).sig_raw
  io.outx.near_path_lza_error := Mux(sel_out, near_path_mods.last.io, near_path_mods.head.io).lza_error
  io.outx.near_path_int_bit := Mux(sel_out, near_path_mods.last.io, near_path_mods.head.io).int_bit
  io.outx.near_path_lzc := Mux(sel_out, near_path_mods.last.io, near_path_mods.head.io).lzc

  io.outx.special_case.valid := special_case_happen
  io.outx.special_case.payload.iv := special_path_iv
  io.outx.special_case.payload.nan := special_path_hasNaN || special_path_inf_iv
  io.outx.special_case.payload.inf_sign := Mux(decode_a.isInf, fp_a.sign, fp_b.sign)

}

// ieee floar: expWidth = 8, precision = 23, outPc = 23
class FCMA_ADD_s2(expWidth: Int, precision: Int, outPc: Int)
  extends TLModule {
  // 1 for the sign bit
  val outputWidth = expWidth + outPc + 1
  val io = new Bundle() {
    val in = slave(new FCMA_ADD_s1_to_s2(expWidth, precision, outPc))
    val result = out port UInt(outputWidth bits)
    val fflags = out port UInt(5 bits)
  }

  val small_add = io.in.small_add

  val special_case = io.in.special_case
  val special_case_happen = special_case.valid
  val special_path_result = Mux(
    special_case.payload.nan,
    Floating.defaultNaNUInt(expWidth, outPc).asBits.asUInt,
    Cat(
      special_case.payload.inf_sign,
      ~U(0, expWidth bits),
      U(0, outPc - 1 bits)
    ).asUInt
  )
  val special_path_iv = special_case.payload.iv
  val special_path_fflags = Cat(special_path_iv, U(0, 4 bits))

  // Far Path
  val far_path_res = cloneOf(io.in.far_path_out)
  far_path_res.sign := io.in.far_path_out.sign

  val adder_in_sig_b = io.in.far_sig_b
  // add extra bits and G, R, S bits
  val adder_in_sig_a = (B"1'b0" ## io.in.far_sig_a ## B"3'b0").asUInt
  val adder_result = adder_in_sig_a + adder_in_sig_b

  val cout = adder_result.msb
  val keep = adder_result.asBits.resizeLeft(2) === B"2'b01"
  val cancellation = adder_result.asBits.resizeLeft(2) === B"2'b00"

  // Mux One Hot
  far_path_res.mantissa := MuxOH(
    // result in this case must fall in [1, 4)
    // if it falls in [1, 2), it does not need further operations
    //                [2, 4),    needs to shift right 1 bit
    Vec(cout, keep || small_add, cancellation && !small_add),
    Vec( // ???
      adder_result.asBits.resizeLeft(outPc + 2) ## adder_result.trim(outPc + 2).orR,  // >> 1
      adder_result.trim(1).asBits.resizeLeft(outPc + 2) ## adder_result.trim(outPc + 3).orR, // keep
      adder_result.trim(2).asBits.resizeLeft(outPc + 2) ## adder_result.trim(outPc + 4).orR // << 1?
    )
  )

  far_path_res.exponent := MuxOH(
    Vec(cout, keep, cancellation),
    io.in.far_exp_a_vec
  ).asBits

  val far_path_exp = far_path_res.exponent
  val far_path_sig = far_path_res.mantissa

  val far_path_tininess_rounder = new TininessRounder(expWidth, outPc)
  far_path_tininess_rounder.io.inx := far_path_res
  far_path_tininess_rounder.io.rm := io.in.rm
  val far_path_tininess = small_add && far_path_tininess_rounder.io.tininess

  val far_path_rounder = RoundingUnit(
    far_path_sig.asUInt.trim(1), // remove the hidden one
    io.in.rm,
    far_path_res.sign,
    outPc
  )

  val far_path_exp_rounded = far_path_rounder.io.cout.asUInt + far_path_exp.asUInt
  val far_path_sig_rounded = far_path_rounder.io.outx

  val far_path_mul_of = io.in.far_path_mul_of
  //  val far_path_may_uf = far_path_out.tininess && !far_path_mul_of
  val far_path_may_uf = far_path_tininess && !far_path_mul_of

  val far_path_of_before_round =
    far_path_exp === B((BigInt(1) << expWidth) - 1)
  val far_path_of_after_round = far_path_rounder.io.cout &&
    far_path_exp === B((BigInt(1) << expWidth) - 2)

  val far_path_of =
    far_path_of_before_round || far_path_of_after_round || far_path_mul_of
  val far_path_ix = far_path_rounder.io.inexact | far_path_of
  val far_path_uf = far_path_may_uf & far_path_ix

  val far_path_result =
    Cat(far_path_res.sign, far_path_exp_rounded, far_path_sig_rounded)

  // val near_path_res = io.in.near_path_out
  val near_path_res = cloneOf(io.in.near_path_out)
  near_path_res.sign := io.in.near_path_out.sign
  val near_path_sign = near_path_res.sign
  val near_path_exp = near_path_res.exponent
  // val near_path_sig = near_path_res.sig
  val near_path_sig_zero = io.in.near_path_sig_is_zero
  val near_path_is_zero = near_path_exp === B(0) && near_path_sig_zero

  val lzc = io.in.near_path_lzc

  val exp_s1 = io.in.near_path_out.exponent.asUInt - lzc
  val exp_s2 = exp_s1 - io.in.near_path_lza_error.asUInt
  near_path_res.exponent := Mux(io.in.near_path_int_bit, exp_s2, U(0)).asBits

  val sig_s1 = (io.in.near_path_sig_raw << lzc)(precision downto 0)
  val sig_s2 = Mux(io.in.near_path_lza_error, Cat(sig_s1.resize(sig_s1.getWidth - 1), U(0, 1 bits)).asUInt, sig_s1)
  val near_path_sig_cor = if (outPc + 3 > precision + 1) {
    Cat(
      sig_s2,
      U(0, outPc + 3 - precision - 1 bits)
    )
  } else {
    sig_s2
  }
  val near_path_sig = near_path_sig_cor(near_path_sig_cor.getWidth - 1 downto near_path_sig_cor.getWidth - (outPc + 2)) ## near_path_sig_cor.resize(near_path_sig_cor.getWidth - (outPc + 2)).orR
  near_path_res.mantissa := near_path_sig

  val near_path_tininess_rounder = new TininessRounder(expWidth, outPc)
  near_path_tininess_rounder.io.inx := near_path_res
  near_path_tininess_rounder.io.rm := io.in.rm
  val near_path_tininess = near_path_tininess_rounder.io.tininess

  val near_path_rounder = RoundingUnit(
    near_path_sig.resize(near_path_sig.getWidth - 1).asUInt,
    io.in.rm,
    near_path_res.sign,
    outPc
  )

  val near_path_exp_rounded = near_path_rounder.io.cout.asUInt + near_path_exp.asUInt
  val near_path_sig_rounded = near_path_rounder.io.outx
  val near_path_zero_sign = io.in.rm === RoundModes().RDN
  val near_path_result = Cat(
    (near_path_sign && !near_path_is_zero) || (near_path_zero_sign && near_path_is_zero),
    near_path_exp_rounded,
    near_path_sig_rounded
  )

  val near_path_of = near_path_exp_rounded === ~U(0, expWidth bits)
  val near_path_ix = near_path_rounder.io.inexact || near_path_of
  val near_path_uf = near_path_tininess && near_path_ix

  val sel_far_path = io.in.sel_far_path
  val common_overflow =
    sel_far_path && far_path_of || !sel_far_path && near_path_of
  val common_overflow_sign =
    Mux(sel_far_path, far_path_res.sign, near_path_res.sign)
  val rmin = RoundingUnit.is_rmin(io.in.rm, far_path_res.sign)
  val common_overflow_exp = Mux(
    rmin,
    U(((BigInt(1) << expWidth) - 2), expWidth bits),
    U(((BigInt(1) << expWidth) - 1), expWidth bits)
  )
  val common_overflow_sig = rmin ?
    B((1 << outPc) - 1, outPc bits) |
    U(0, outPc bits).asBits

  val common_underflow =
    sel_far_path && far_path_uf || !sel_far_path && near_path_uf
  val common_inexact =
    sel_far_path && far_path_ix || !sel_far_path && near_path_ix
  val common_fflags = Cat(
    False,
    False,
    common_overflow,
    common_underflow,
    common_inexact
  )

  io.result := Mux(
    special_case_happen,
    special_path_result,
    Mux(
      common_overflow,
      Cat(common_overflow_sign, common_overflow_exp, common_overflow_sig),
      Mux(sel_far_path, far_path_result, near_path_result)
    ).asUInt
  )
  io.fflags := Mux(special_case_happen, special_path_fflags, common_fflags).asUInt
}

class FCMA_ADD(val expWidth: Int, val precision: Int, val outPc: Int) extends TLModule {
  // 1 for the sign bit
  val inputWidth = expWidth + precision + 1
  val outputWidth = expWidth + outPc + 1
  val io = new Bundle() {
    val a, b = in port UInt(inputWidth bits)
    val rm = in port UInt(3 bits)
    val result = out port UInt(outputWidth bits)
    val fflags = out port UInt(5 bits)
  }

  val fadd_s1 = new FCMA_ADD_s1(expWidth, precision, outPc)
  val fadd_s2 = new FCMA_ADD_s2(expWidth, precision, outPc)

  fadd_s1.io.a := io.a
  fadd_s1.io.b := io.b
  fadd_s1.io.rm := io.rm

  fadd_s2.io.in := fadd_s1.io.outx

  io.result := fadd_s2.io.result
  io.fflags := fadd_s2.io.fflags
}


class FADD(val expWidth: Int, val precision: Int) extends TLModule {
  // 1 for the sign bit
  val inputWidth = expWidth + precision + 1
  val io = new Bundle() {
    val a, b = in port UInt(inputWidth bits)
    val rm = in port UInt(3 bits) // round mode
    val result = out port UInt(inputWidth bits)
    val fflags = out port UInt(5 bits)
  }

  val module = new FCMA_ADD(expWidth, precision, precision)

  module.io.a := io.a
  module.io.b := io.b
  module.io.rm := io.rm
  io.result := module.io.result
  io.fflags := module.io.fflags
}
