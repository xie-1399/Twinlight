package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random

class LZATest extends AnyFunSuite {

  def clz_std(a: Array[Boolean]): Int = {
    var std_o = -1
    for (i <- a.indices) {
      if (a.reverse(i) && std_o == -1) {
        std_o = i
      }
    }
    std_o
  }

  test(" LZA random test") {
    SIMCFG(gtkFirst = true).compile {
      val dut = new LZA(28)
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        SimTimeout(1000000 * 10)

        def monitor() = {
          val testThread = fork {
            val gen = new Random()
            val err = Array.tabulate(10)({ i =>
              dut.io.a #= gen.nextInt(0x7fffffff >> (i % 28 + 3))
              dut.io.b #= gen.nextInt(0x7fffffff >> (i % 28 + 3))
              dut.clockDomain.waitSampling()
              val o_str = dut.io.f.toInt.toBinaryString.reverse.padTo(28, '0').reverse

              val s = dut.io.a.toInt + dut.io.b.toInt
              val s_str = s.toBinaryString.reverse.padTo(28, '0').reverse

              val std_diff = (clz_std(Array.tabulate(28)({ c =>
                if (s_str.reverse(c) == '0') false else true
              })) - clz_std(Array.tabulate(28)({ c =>
                if (o_str.reverse(c) == '0') false else true
              }))).abs
              std_diff <= 1
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
