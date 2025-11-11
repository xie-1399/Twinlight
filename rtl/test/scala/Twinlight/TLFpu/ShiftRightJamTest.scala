package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random

class ShiftRightJamTest extends AnyFunSuite {

  test(" ShiftRightJam random test") {
    SIMCFG(gtkFirst = true).compile {
      val dut = new ShiftRightJam(32, 8)
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        SimTimeout(1000000 * 10)

        def monitor() = {
          val testThread = fork {
            val gen = new Random()
            val err = Array.tabulate(32767)({ i =>
              dut.io.i #= gen.nextInt(0x7fffffff)
              var lb = 0
              var ub = 32
              if (i < 10000) {
                lb = 0
                ub = 32
              } else if (i < 15000) {
                lb = 32
                ub = 64
              } else {
                lb = 0
                ub = (1 << 8) - 1
              }
              dut.io.shamt #= gen.nextInt(ub - lb) + lb
              dut.clockDomain.waitSampling()
              val o = dut.io.o.toInt
              val s = dut.io.sticky.toBoolean

              val std_o = if (dut.io.shamt.toInt >= 32) 0 else dut.io.i.toInt >>> dut.io.shamt.toInt
              val std_sticky = Array.tabulate(32)({ i =>
                if (i < dut.io.shamt.toInt) dut.io.i.toBooleans(i) else false
              }).reduce(_ | _)
              (std_o == o) && (std_sticky == s)
            }).map { i => if (i) 0 else 1 }.sum

            if (err == 0) {
              simSuccess()
            } else {
              simFailure(s"$err errors found!!!")
            }
          }
        }

        monitor()
    }
  }

}
