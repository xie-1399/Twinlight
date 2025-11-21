package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random

case class BasicFloatTools() {
  // subnormal: s 00000000 f
  // inf      : s 11111111 ==0
  // NaN      : s 11111111 !=0
  def FP2Int(x: Float): Int = {
    // only 32 bits version now
    java.lang.Float.floatToRawIntBits(x)
  }

  def Int2FP(x: Int): Float = {
    java.lang.Float.intBitsToFloat(x)
  }

  def isNan(x: Float): Boolean = {
    val xint = FP2Int(x)
    isNan(xint)
  }

  def isNan(x: Int): Boolean = {
    val xint = x
    val exp = (xint & 0x7F800000) >> 23
    val mantissa = xint & 0x007FFFFF
    exp == 0xFF && mantissa != 0
  }

  def genRand(): (BigInt, Float) = {
    val gen = new Random()
    val randi = gen.nextInt(100)

    val rand_percent = 70
    val inf_percent = 10
    val nan_percent = 10
    val subnormal_percent = 10

    assert(rand_percent + inf_percent + nan_percent + subnormal_percent == 100)

    val rand_threshold = 0 + rand_percent
    val inf_threshold = rand_threshold + inf_percent
    val nan_threshold = inf_threshold + nan_percent
    val subnormal_threshold = nan_threshold + subnormal_percent

    assert(subnormal_threshold == 100)

    val xint = (if (randi < rand_threshold) { // normal
      BigInt((if (gen.nextBoolean()) -1 else 1) * gen.nextInt(Int.MaxValue))
    } else if (randi < inf_threshold) { // Inf
      BigInt(if (gen.nextBoolean()) "7F800000" else "FF800000", 16)
    } else if (randi < nan_threshold) { // NaN
      BigInt(if (gen.nextBoolean()) "7F800000" else "FF800000", 16) + gen.nextInt((1 << 23) - 1) + 1
    } else { // subnormal
      BigInt(if (gen.nextBoolean()) "00000000" else "80000000", 16) + gen.nextInt((1 << 23) - 1) + 1
    }).mod(BigInt(1) << 32)
    val xfloat = Int2FP(xint.toInt)
    (xint, xfloat)
  }
}

class Fadd32Test extends AnyFunSuite {

  test("Fadd32 random test") {
    SIMCFG().compile {
      val dut = FPU_ADD(expWidth = 8, precision = 23)
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        //        SimTimeout(100000000 * 10)

        def monitor() = {
          val testThread = fork {
            val testCase = 1 << 10
            val tool = BasicFloatTools()
            val err = Array.tabulate(testCase)({ i =>
              val (a, fa) = tool.genRand()
              val (b, fb) = tool.genRand()
              dut.io.a #= a
              dut.io.b #= b
              dut.io.rm #= 0
              dut.clockDomain.waitSampling(1)
              // check
              val res = dut.io.result.toInt
              val std_res = tool.FP2Int(fa + fb)
              if ((res != std_res) && !(tool.isNan(res) && tool.isNan(std_res))) {
                println(s"a + b = $fa + $fb = $a + $b = ${tool.Int2FP(std_res)} = $std_res")
                println(s"res   = $fa + $fb = $a + $b = ${tool.Int2FP(res)} = $res")
                println(" ")
              }

              (res == std_res) || (tool.isNan(res) && tool.isNan(std_res))
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
