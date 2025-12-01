package Twinlight.TLFpu

import spinal.core._

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