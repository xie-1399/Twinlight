package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random

class CLZTest extends AnyFunSuite {

  test(" CLZ random test") {
    SIMCFG(gtkFirst = true).compile {
      val dut = new CLZ(28)
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        SimTimeout(1000000 * 10)

        def monitor() = {
          val testThread = fork {
            val gen = new Random()
            val err = Array.tabulate(32767)({ i =>
              dut.io.clz_in #= gen.nextInt(0x7fffffff >> (i % 28 + 3))
              dut.clockDomain.waitSampling()
              val o = dut.io.clz_out.toInt
              var std_o = -1;
              for (i <- dut.io.clz_in.toBooleans.indices) {
                if (dut.io.clz_in.toBooleans.reverse(i) && std_o == -1) {
                  std_o = i
                }
              }
              if (!dut.io.clz_in.toBooleans.contains(true)) true else std_o == o
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
