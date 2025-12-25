package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.language.postfixOps

class FPMULTest extends AnyFunSuite {

  test("FPMUL random test") {
    SIMCFG().compile {
      val dut = if (BasicFloatTools().IEEE_FP32) {
        FMUL(expWidth = 8, precision = 23)
      } else {
        FMUL(expWidth = 5, precision = 10)
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
//              val sa = "547"
//              val sb = "8023"
//              val base = 16
//
//              val (a, fa) = (BigInt(sa, base).mod(1L << 16), new FP16().int16tofloat(BigInt(sa, base).toInt))
//              val (b, fb) = (BigInt(sb, base).mod(1L << 16), new FP16().int16tofloat(BigInt(sb, base).toInt))

              dut.io.a #= a
              dut.io.b #= b
              dut.io.rm #= RoundingEncoding.RNE
              dut.clockDomain.waitSampling(1)
              // check
              val res = dut.io.result.toInt
              val std_res = if (BasicFloatTools().IEEE_FP32) tool.FP2Int(fa * fb) else new FP16().float2int16(fa * fb)

              //              println(s"fa + fb = ${fa + fb}")

              val res_f = if (BasicFloatTools().IEEE_FP32) tool.Int2FP(res) else new FP16().int16tofloat(res)
              val std_res_f = if (BasicFloatTools().IEEE_FP32) tool.Int2FP(std_res) else new FP16().int16tofloat(std_res)

              val rerr_tmp = (if (BasicFloatTools().IEEE_FP32) (tool.Int2FP(res) - tool.Int2FP(std_res)) / tool.Int2FP(std_res) else (new FP16().int16tofloat(res) - new FP16().int16tofloat(std_res)) / new FP16().int16tofloat(std_res)).abs
              val rerr = if (rerr_tmp.isNaN || rerr_tmp.isInfinity) 0.0f else rerr_tmp
              val iseq = (res == std_res) || (tool.isNan(res) && tool.isNan(std_res))
              val s_failed = !iseq && rerr > epsilon
              if (!iseq && rerr > epsilon) {
                println(s"a * b = $fa * $fb = 0x${a.toInt.toHexString} * 0x${b.toInt.toHexString} = $std_res_f = 0x${std_res.toHexString}")
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
