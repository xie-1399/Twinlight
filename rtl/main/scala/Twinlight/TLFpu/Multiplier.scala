package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object SignExt {
  def apply(a: UInt, len: Int): UInt = {
    val aLen = a.getWidth
    val signBit = a.msb
    if (aLen >= len) a.resize(len) else ((signBit #* (len - aLen)) ## a).asUInt
  }
}

case class Multiplier(len: Int, pipeAt: Seq[Int]) extends TLModule {
  val io = new Bundle {
    // input a and b are signed integers. Thus the param len includes a sign bit.
    val a, b = in port UInt(len bits)
    val regEnables = in port Vec.fill(pipeAt.size)(Bool())
    val result = out port UInt((2 * len) bits)
    val sum = out port UInt(2 * len bits)
    val carry = out port UInt(2 * len bits)
  }
  val (a, b) = (io.a, io.b)
  // input mantissas with the hidden one

  val b_sext, bx2, neg_b, neg_bx2 = UInt(len + 1 bits)
  b_sext := SignExt(b, len + 1)
  // len(bx2) == len + 1, to make sure that it wouldn't overflow
  bx2 := b_sext |<< 1
  neg_b := ~b_sext
  neg_bx2 := neg_b |<< 1

  val columns: Array[Seq[Bool]] = Array.fill(2 * len)(Seq())

  var last_x = U"3'b0"
  for (i <- Range(0, len, 2)) {
    val x = if (i == 0) {
      a(1 downto 0) @@ U"1'b0"
    } else if (i + 1 == len) {
      SignExt(a(i downto i - 1), 3)
    } else {
      a(i + 1 downto i - 1)
    }
    val pp_temp = UInt(len + 1 bits)
    switch(x) {
      is(U(1)) {
        pp_temp := b_sext
      }
      is(U(2)) {
        pp_temp := b_sext
      }
      is(U(3)) {
        pp_temp := bx2
      }
      is(U(4)) {
        pp_temp := neg_bx2
      }
      is(U(5)) {
        pp_temp := neg_b
      }
      is(U(6)) {
        pp_temp := neg_b
      }
      default {
        pp_temp := U(0, len + 1 bits)
      }
    }
    val s = pp_temp.msb
    val t = UInt(2 bits)
    switch(last_x) {
      is(U(4)) {
        t := U"2'h2"
      }
      is(U(5)) {
        t := U"2'h1"
      }
      is(U(6)) {
        t := U"2'h1"
      }
      default {
        t := U"2'h0"
      }
    }
    last_x = x
    val (pp, weight) = i match {
      case 0 =>
        (Cat(~s, s, s, pp_temp), 0)
      case n if (n == len - 1) || (n == len - 2) =>
        (Cat(~s, pp_temp, t), i - 2)
      case _ =>
        (Cat(U"1'b1", ~s, pp_temp, t), i - 2)
    }

    for (j <- columns.indices) {
      if (j >= weight && j < (weight + pp.getWidth)) {
        columns(j) = columns(j) :+ pp(j - weight)
      }
    }

  }

  def addOneColumn(col: Seq[Bool], cin: Seq[Bool]): (Seq[Bool], Seq[Bool], Seq[Bool]) = {
    var sum = Seq[Bool]()
    var cout1 = Seq[Bool]()
    var cout2 = Seq[Bool]()
    col.size match {
      case 1 => // do nothing
        sum = col ++ cin
      case 2 =>
        val c22 = C22()
        c22.io.inx := Vec.tabulate(2)({ i => col(i).asUInt })
        sum = c22.io.outx(0).asBool +: cin
        cout2 = Seq(c22.io.outx(1).asBool)
      case 3 =>
        val c32 = C32()
        c32.io.inx := Vec.tabulate(3)({ i => col(i).asUInt })
        sum = c32.io.outx(0).asBool +: cin
        cout2 = Seq(c32.io.outx(1).asBool)
      case 4 =>
        val c53 = C53()
        for ((x, y) <- c53.io.inx.take(4) zip col) {
          x := y.asUInt
        }
        c53.io.inx.last := (if (cin.nonEmpty) cin.head.asUInt else U(0))
        sum = Seq(c53.io.outx(0).asBool) ++ (if (cin.nonEmpty) cin.drop(1) else Nil)
        cout1 = Seq(c53.io.outx(1).asBool)
        cout2 = Seq(c53.io.outx(2).asBool)
      case n =>
        val cin_1 = if (cin.nonEmpty) Seq(cin.head) else Nil
        val cin_2 = if (cin.nonEmpty) cin.drop(1) else Nil
        val (s_1, c_1_1, c_1_2) = addOneColumn(col take 4, cin_1)
        val (s_2, c_2_1, c_2_2) = addOneColumn(col drop 4, cin_2)
        sum = s_1 ++ s_2
        cout1 = c_1_1 ++ c_2_1
        cout2 = c_1_2 ++ c_2_2
    }
    (sum, cout1, cout2)
  }

  def addAll(cols: Array[Seq[Bool]], depth: Int): (UInt, UInt) = {
    if (cols.map(_.size).max <= 2) {
      val sum = Cat(cols.map(_.head))
      var k = 0
      while (cols(k).size == 1) {
        k = k + 1
      }
      val carry = Cat(cols.drop(k).map(_(1)))//U(0, cols.length - k bits) //
      (sum.asUInt, (carry ## U(0, k bits)).asUInt)
    } else {
      val columns_next = Array.fill(2 * len)(Seq[Bool]())
      var cout1, cout2 = Seq[Bool]()
      for (i <- cols.indices) {
        val (s, c1, c2) = addOneColumn(cols(i), cout1)
        columns_next(i) = s ++ cout2
        cout1 = c1
        cout2 = c2
      }

      //      val needReg = pipeAt.contains(depth)
      val toNextLayer = columns_next
      addAll(toNextLayer, depth + 1)
    }
  }

  val (sum, carry) = addAll(cols = columns, depth = 0)

  io.sum := sum
  io.carry := carry
  io.result := (sum.expand + carry.expand).trim(1)
}