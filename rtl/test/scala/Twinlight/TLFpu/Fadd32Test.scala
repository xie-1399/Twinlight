package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random

case class BasicFloatTools() {
  val IEEE_FP32 = false
  val IEEE_FP16 = true

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

    val (xint, xfloat) = if (IEEE_FP32) {
      val xx = (if (randi < rand_threshold) { // normal
        BigInt((if (gen.nextBoolean()) -1 else 1) * gen.nextInt(Int.MaxValue))
      } else if (randi < inf_threshold) { // Inf
        BigInt(if (gen.nextBoolean()) "7F800000" else "FF800000", 16)
      } else if (randi < nan_threshold) { // NaN
        BigInt(if (gen.nextBoolean()) "7F800000" else "FF800000", 16) + gen.nextInt((1 << 23) - 1) + 1
      } else { // subnormal
        BigInt(if (gen.nextBoolean()) "00000000" else "80000000", 16) + gen.nextInt((1 << 23) - 1) + 1
      }).mod(BigInt(1) << 32)
      (xx, Int2FP(xx.toInt))
    } else {
      val xx = (if (randi < rand_threshold) { // normal
        BigInt((if (gen.nextBoolean()) -1 else 1) * gen.nextInt(Short.MaxValue))
      } else if (randi < inf_threshold) { // Inf
        BigInt(if (gen.nextBoolean()) "7C00" else "FC00", 16)
      } else if (randi < nan_threshold) { // NaN
        BigInt(if (gen.nextBoolean()) "7C00" else "FC00", 16) + gen.nextInt((1 << 10) - 1) + 1
      } else { // subnormal
        BigInt(if (gen.nextBoolean()) "0000" else "8000", 16) + gen.nextInt((1 << 10) - 1) + 1
      }).mod(BigInt(1) << 16)
      (xx, new FP16().int16tofloat(xx.toInt))
    }
    //    println((if (IEEE_FP32) "xint32 = " else "xint16 = ") + xint)
    //    println((if (IEEE_FP32) "fp32 = " else "fp16 = ") + xfloat.toString)
    (xint, xfloat)
  }
}

class Fadd32Test extends AnyFunSuite {

  test("Fadd32 random test") {
    SIMCFG().compile {
      val dut = if (BasicFloatTools().IEEE_FP32) {
        FPU_ADD(expWidth = 8, precision = 23)
      } else {
        FPU_ADD(expWidth = 5, precision = 10)
      }
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
              val (a, fa) = tool.genRand()
              val (b, fb) = tool.genRand()
              //              val (a, fa) = (BigInt("0", 16).mod(1L << 16), new FP16().int16tofloat(BigInt("0", 16).toInt))
              //              val (b, fb) = (BigInt("13a8", 16).mod(1L << 16), new FP16().int16tofloat(BigInt("13a8", 16).toInt))
              dut.io.a #= a
              dut.io.b #= b
              dut.io.rm #= RoundingEncoding.RNE
              dut.clockDomain.waitSampling(1)
              // check
              val res = dut.io.result.toInt
              val std_res = if (BasicFloatTools().IEEE_FP32) tool.FP2Int(fa + fb) else new FP16().float2int16(fa + fb)

              //              println(s"fa + fb = ${fa + fb}")

              val res_f = if (BasicFloatTools().IEEE_FP32) tool.Int2FP(res) else new FP16().int16tofloat(res)
              val std_res_f = if (BasicFloatTools().IEEE_FP32) tool.Int2FP(std_res) else new FP16().int16tofloat(std_res)

              val rerr_tmp = (if (BasicFloatTools().IEEE_FP32) (tool.Int2FP(res) - tool.Int2FP(std_res)) / tool.Int2FP(std_res) else (new FP16().int16tofloat(res) - new FP16().int16tofloat(std_res)) / new FP16().int16tofloat(std_res)).abs
              val rerr = if (rerr_tmp.isNaN || rerr_tmp.isInfinity) 0.0f else rerr_tmp
              val iseq = (res == std_res) || (tool.isNan(res) && tool.isNan(std_res))
              val s_failed = !iseq && rerr > epsilon
              if (!iseq && rerr > epsilon) {
                println(s"a + b = $fa + $fb = 0x${a.toInt.toHexString} + 0x${b.toInt.toHexString} = $std_res_f = 0x${std_res.toHexString}")
                println(s"res     ${"".padTo((fa.toString.length + fb.toString.length + 12 + a.toString().length + b.toString().length), ' ')} = $res_f = 0x${res.toHexString}")
                println(" ")
              }
              (rerr, iseq, s_failed)
            }).map({ x => if (x._2) 0.0f else if (x._3) 2 * epsilon * testCase else x._1 }).sum / testCase

            if (err < epsilon) {
              println(s"Passed!!! E(rerr) = $err < $epsilon")
              simSuccess()
            } else {
              simFailure(s"Failed!!! E(rerr) = $err >= $epsilon")
            }
          }
        }

        monitor()
    }
  }

}
