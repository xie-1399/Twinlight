package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random

class MultiplierTest extends AnyFunSuite {

  test("MultiplierTest random test") {
    SIMCFG().compile {
      val dut = if (BasicFloatTools().IEEE_FP32) {
        Multiplier(23 + 1 + 1, Seq())
      } else {
        Multiplier(10 + 1 + 1, Seq())
      }
      dut.columns.foreach(_.foreach(_.simPublic()))
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)

        //        SimTimeout(100000000 * 10)
        def monitor() = {
          val testThread = fork {
            val testCase = 1 << 20
            val epsilon = 1.0 * 1e-2
            val tool = BasicFloatTools()
            val err = Array.tabulate(testCase)({ i =>
              val a = tool.gen.nextInt(1 << 11)
              val b = tool.gen.nextInt(1 << 11)
              dut.io.a #= a
              dut.io.b #= b
              dut.io.regEnables.foreach(_ #= true)
              dut.clockDomain.waitSampling(1)
              // check

              val res = dut.io.result.toInt

              val std_res = a * b

              val iseq = res == std_res
              if (!iseq) {
                println(s"a * b = $a * $b = 0x${a.toHexString} * 0x${b.toHexString} = $std_res = 0x${std_res.toHexString}")
                println(s"res     ${"".padTo((a.toString.length + b.toString.length + 12 + a.toString().length + b.toString().length), ' ')} = $res = 0x${res.toHexString}")
                println(" ")
              }
              if (iseq) 0 else 1
            }).sum

            if (err == 0) {
              println(s"Passed!!!")
              simSuccess()
            } else {
              simFailure(s"Failed!!! error = $err")
            }
          }
        }

        monitor()
    }
  }

}
