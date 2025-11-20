package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random

case class BasicFloatTools() {
  def FP2Int(x: Float): Int = {
    // only 32 bits version now
    java.lang.Float.floatToRawIntBits(x)
  }

  def Int2FP(x: Int): Float = {
    java.lang.Float.intBitsToFloat(x)
  }
}

class Fadd32Test extends AnyFunSuite {

  test("Fadd32 random test") {
    SIMCFG().compile {
      val dut = new FADD(expWidth = 8, precision = 23)
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        //        SimTimeout(100000000 * 10)

        def monitor() = {
          val testThread = fork {
            val testCase = 1 << 20
            val gen = new Random()
            val tool = BasicFloatTools()
            val err = Array.tabulate(testCase)({ i =>
              val a = gen.nextFloat() * (1024000111 >> ((i + 0) % 32))
              val b = gen.nextFloat() * (1024000111 >> ((i + 7) % 32))
              dut.io.a #= tool.FP2Int(a)
              dut.io.b #= tool.FP2Int(b)
              dut.io.rm #= 0
              dut.clockDomain.waitSampling(2)
              // check
              val res = dut.io.result.toBigInt
              val std_res = tool.FP2Int(a + b)
              //              println(s"a + b = $a + $b = ${tool.Int2FP(std_res)}")
              //              println(s"res   = ${tool.Int2FP(res.toInt)}")
              res == std_res
            }).map({ x => if (x) 0 else 1 }).sum

            if (err == 0) {
              println(s"${testCase - err}/$testCase passed!!!")
              simSuccess()
            } else {
              simFailure(s"$err/$testCase errors found!!!")
            }
          }
        }

        monitor()
    }
  }

}
