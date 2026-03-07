package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random

class LbCeilTest extends AnyFunSuite {

  test(" LbCeil random test") {
    val len = 12
    SIMCFG(gtkFirst = true).compile {
      val dut = Lb_ceil(intWidth = len)
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        def monitor() = {
          val testThread = fork {
            val gen = new Random()
            val err = Array.tabulate(1)({ i =>
              dut.io.a.randomize()
              dut.clockDomain.waitSampling()
              val inx = dut.io.a.toInt
              val o = dut.io.o.toInt
              val std_o = if (inx == 0) 0 else (math.log(inx) / math.log(2)).floor.toInt
              if (std_o != o){
                println(inx, std_o, o)
              }
              std_o == o
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
