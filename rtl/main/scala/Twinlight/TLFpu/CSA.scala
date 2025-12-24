package Twinlight.TLFpu

import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

abstract class CarrySaveAdderMToN(m: Int, n: Int)(len: Int) extends TLModule {
  val io = new Bundle {
    val inx = in port Vec.fill(m)(UInt(len bits))
    val outx = out port Vec.fill(n)(UInt(len bits))
  }
}

// TODO: coding style optimization required.
class CSA2_2(len: Int) extends CarrySaveAdderMToN(2, 2)(len) {
  val (a, b) = (io.inx(0), io.inx(1))
  val sum = a ^ b
  val cout = a & b

  io.outx(0) := sum
  io.outx(1) := cout
}

class CSA3_2(len: Int) extends CarrySaveAdderMToN(3, 2)(len) {
  val (a, b, cin) = (io.inx(0), io.inx(1), io.inx(2))
  val a_xor_b = a ^ b
  val a_and_b = a & b
  val sum = a_xor_b ^ cin
  val cout = a_and_b | (a_xor_b & cin)

  io.outx(0) := sum
  io.outx(1) := cout
}

class CSA5_3(len: Int) extends CarrySaveAdderMToN(5, 3)(len) {
  val FAs = Array.fill(2)(new CSA3_2(len))
  //                                  in0, in1, in2
  FAs(0).io.inx := Vec.tabulate(3)({i => io.inx(i)})
  //                         sum          in3          cin
  FAs(1).io.inx := Vec(FAs(0).io.outx(0), io.inx(3), io.inx(4))
  io.outx(0) := FAs(1).io.outx(0) // sum
  io.outx(1) := FAs(0).io.outx(1) // <<1
  io.outx(2) := FAs(1).io.outx(1) // <<2
}

case class C22() extends CSA2_2(1)
case class C32() extends CSA3_2(1)
case class C53() extends CSA5_3(1)
