package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class FPSpecial extends Bundle {
  val isNaN = Bool()
  val isInf = Bool()
  val isInv = Bool()
  val overflow = Bool()
}

class FarPath(expWidth: Int, outPC: Int) extends TLModule {
  val io = new Bundle {
    val a = in port new RawFloat(expWidth, outPC)
    val b = in port new RawFloat(expWidth, outPC)
    val exp_diff = in port UInt(expWidth bits)
    val expSub = in port Bool()

    val a_mantissa = out port UInt(outPC bits)
    val b_mantissa = out port UInt(outPC + 3 + 1 bits) // manWidth + G, R, S bits + sign bit
    val b_sticky = out port Bool()
    val a_exps = out port Vec.fill(3)(UInt(expWidth bits))
    val result = out port new RawFloat(expWidth, outPC + 3)
  }
  val (b_shifted, b_sticky) = ShiftRightJam(io.b.mantissa ## B(0, 2 bits), io.exp_diff)

  val b_mantissa_raw = B(0, 1 bits) ## b_shifted ## b_sticky
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

class NearPath(expWidth: Int, outPC: Int) extends TLModule {
  val io = new Bundle {
    val a = in port new RawFloat(expWidth, outPC)
    val b = in port new RawFloat(expWidth, outPC)
    val need_shift_b = in port Bool()
    val rm = in port UInt(3 bits)

    val result = out port new RawFloat(expWidth, outPC + 3)
    val sig_is_zero = out port Bool()
    val a_lt_b = out port Bool()
    val lza_error = out port Bool()
    val int_bit = out port Bool()
    val sig_raw = out port UInt(outPC + 1 bits)
    val lzc = out port UInt(log2Up(outPC + 1) bits)
  }

  val a_sig = (io.a.mantissa ## B(0, 1 bits)).asUInt
  val b_sig = ((io.b.mantissa ## U(0, 1 bits)) >> io.need_shift_b.asUInt).asUInt
  val b_neg = (~b_sig.asBits).asUInt

  val a_minus_b = (U(0, 1 bits) ## a_sig).asUInt + (U(1, 1 bits) ## b_neg).asUInt + U(1, 1 + a_sig.getWidth bits)
  val a_lt_b = a_minus_b.lsb
  val sig_raw = a_minus_b.asBits(outPC downto 0).asUInt
  val lza_str = LZA(outPC + 1, a_sig, b_neg)
  val lza_str_zero = !lza_str.orR

  val need_shift_lim = io.a.exponent.asUInt < U(outPC + 1, io.a.exponent.getWidth bits)
  val mask_able_k_width = log2Up(outPC + 1)
  val shift_lim_mask_raw = ((B(1, 1 bits) ## B(0, outPC + 1 bits)) >> io.a.exponent(mask_able_k_width - 1 downto 0).asUInt).asUInt(outPC downto 0)
  val shift_lim_mask = Mux(need_shift_lim, shift_lim_mask_raw, U(0, shift_lim_mask_raw.getWidth bits))
  val shift_lim_bit = (shift_lim_mask_raw & sig_raw).orR

  val lzc_str = shift_lim_mask | lza_str
  val lzc = CLZ(lzc_str)

  val int_bit_mask = Vec.tabulate(outPC + 1)({ i =>
    if (i == outPC) {
      lzc_str(i)
    } else {
      lzc_str(i) & !lzc_str(outPC - 1 - i downto 0).orR
    }
  })

  val int_bit_predicted = (int_bit_mask.map({ i => i | lza_str_zero }).asBits & sig_raw.asBits).orR
  val int_bit_rshift_1 = ((int_bit_mask.asBits >> U(1, 1 bits)).asUInt & sig_raw).orR

  val exceed_lim_mask = Vec.tabulate(outPC + 1)({ i =>
    if (i == outPC) {
      False
    } else {
      lza_str(outPC - 1 - i downto 0).orR
    }
  })

  val exceed_lim = need_shift_lim && !(exceed_lim_mask.asBits.asUInt & shift_lim_mask_raw).orR
  val int_bit = Mux(exceed_lim, shift_lim_bit, int_bit_rshift_1 || int_bit_predicted)

  val lza_error = !int_bit_predicted && !exceed_lim

  val near_path_sign = Mux(a_lt_b, io.b.sign, io.a.sign)

  val result = new RawFloat(expWidth, outPC + 3)
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
