package Twinlight.TLPE

import Twinlight.TLSim._
import Twinlight.TLFpu._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.language.postfixOps

class PETopTest extends AnyFunSuite {

  val tool = BasicFloatTools(fp32 = false, fp16 = true)

  val testConfigs = Array(
    (true, true),
    (true, false),
    (false, true),
    (false, false),
  )
  testConfigs.foreach { testConfig =>
    test(s"PETopTest(is_wallace = ${testConfig._1}, is_bitslice = ${testConfig._2}) random test") {
      SIMCFG().compile {
        val dut = if (tool.IEEE_FP32) {
          PE_top(weightWidth = 8, actExpWidth = 8, actMantissaWidth = 23, psumExpWidth = 8, psumMantissaWidth = 23, is_wallace = testConfig._1, is_bitslice = testConfig._2)
        } else {
          PE_top(weightWidth = 8, actExpWidth = 5, actMantissaWidth = 10, psumExpWidth = 5, psumMantissaWidth = 10, is_wallace = testConfig._1, is_bitslice = testConfig._2)
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
              val err = Array.tabulate(testCase)({ i =>
                val (b, fb) = tool.genRand()
                val (d, fd) = tool.genRand()

                dut.io.in_a.randomize()
                dut.io.rm #= RoundingEncoding.RNE
                dut.io.a_preload #= true
                dut.clockDomain.waitSampling(1)

                // preloaded a
                dut.io.in_b #= b
                dut.io.in_d #= d
                dut.io.a_preload #= false
                dut.clockDomain.waitSampling(1)

                val a = dut.io.in_a.toInt
                val fa = a.toFloat

                // check
                val res = dut.io.out_c.toInt

                // to avoid that no errors are introduced under all-fp32 precision
                val std_res_temp = if (tool.IEEE_FP32) tool.FP2Int(fa * fb) else new FP16().float2int16(fa * fb)
                val std_res = if (tool.IEEE_FP32) tool.FP2Int(tool.Int2FP(std_res_temp) + fd) else new FP16().float2int16(new FP16().int16tofloat(std_res_temp) + fd)

                //              println(s"fa + fb = ${fa + fb}")

                val res_f = if (tool.IEEE_FP32) tool.Int2FP(res) else new FP16().int16tofloat(res)
                val std_res_f = if (tool.IEEE_FP32) tool.Int2FP(std_res) else new FP16().int16tofloat(std_res)

                val rerr_tmp = (if (tool.IEEE_FP32) (tool.Int2FP(res) - tool.Int2FP(std_res)) / tool.Int2FP(std_res) else (new FP16().int16tofloat(res) - new FP16().int16tofloat(std_res)) / new FP16().int16tofloat(std_res)).abs
                val rerr = if (rerr_tmp.isNaN || rerr_tmp.isInfinity) 0.0f else rerr_tmp
                val iseq = (res == std_res) || (tool.isNan(res) && tool.isNan(std_res))
                val s_failed = !iseq
                if (!iseq) {
                  println(s"a * b + d = $fa * $fb + $fd = 0x${a.toInt.toHexString} * 0x${b.toInt.toHexString} + 0x${d.toInt.toHexString} = $std_res_f = 0x${std_res.toHexString}")
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


}
