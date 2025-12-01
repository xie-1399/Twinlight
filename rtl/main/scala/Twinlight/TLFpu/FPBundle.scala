package Twinlight.TLFpu

import spinal.core._

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