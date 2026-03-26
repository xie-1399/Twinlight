package Twinlight.TLPE

import Twinlight.TLSim._
import Twinlight.TLFpu._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.language.postfixOps

class PETopTest extends AnyFunSuite {

  val tool = BasicFloatTools(fp32 = false, fp16 = true)
  val weightBufferSize = 4
  val require_bias = false

  def fpPrecisionPreserve(a: Float): Float = {
    if (tool.IEEE_FP32) tool.Int2FP(tool.FP2Int(a)) else new FP16().int16tofloat(new FP16().float2int16(a))
  }

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
          PE_top(require_bias=require_bias, weightBufferSize = weightBufferSize, weightWidth = 8, actExpWidth = 8, actMantissaWidth = 23, psumExpWidth = 8, psumMantissaWidth = 23, is_wallace = testConfig._1, is_bitslice = testConfig._2)
        } else {
          PE_top(require_bias=require_bias, weightBufferSize = weightBufferSize, weightWidth = 8, actExpWidth = 5, actMantissaWidth = 10, psumExpWidth = 5, psumMantissaWidth = 10, is_wallace = testConfig._1, is_bitslice = testConfig._2)
        }
        dut
      }.doSimUntilVoid {
        dut =>
          dut.clockDomain.forkStimulus(10)

          //        SimTimeout(100000000 * 10)
          def monitor() = {
            val testThread = fork {
              val testCase = 1 << 15
              val epsilon = 1.0 * 1e-2
              val err = Array.tabulate(testCase)({ i =>
                dut.io.in_a.foreach(_.randomize())
                dut.io.rm #= RoundingEncoding.RNE
                dut.io.a_preload #= true
                dut.io.calc_valid #= false
                dut.clockDomain.waitSampling(1)
                val avec = Array.tabulate(weightBufferSize) { x =>
                  dut.io.in_a(x).toInt
                }
                val favec = avec.map(_.toFloat)

                val bBF = Array.tabulate(weightBufferSize) { x =>
                  tool.genRandAlmostNormal()
                }
                val bvec = bBF.map(_._1)
                val fbvec = bBF.map(_._2)

                val (d, fd) = tool.genRandAlmostNormal()

                // to calculate C = A * W + d
                (0 until weightBufferSize).foreach { idx =>
                  val b = bvec(idx)
                  // preloaded a
                  dut.io.in_b #= b
                  if (require_bias) {
                    dut.io.in_d #= d
                  }
                  dut.io.a_preload #= false
                  dut.io.calc_valid #= true
                  dut.clockDomain.waitSampling(1)
                  dut.clockDomain.waitFallingEdge()
                }

                // check
                val res = dut.io.out_c.toInt

                // to avoid that no errors are introduced under all-fp32 precision
                var std_res_f = if (require_bias) fd else 0
                Array.tabulate(weightBufferSize) { x =>
                  fpPrecisionPreserve(favec(x) * fbvec(x))
                }.foreach { x =>
                  std_res_f = fpPrecisionPreserve(std_res_f + x)
                }

                val std_res = if (tool.IEEE_FP32) tool.FP2Int(std_res_f) else new FP16().float2int16(std_res_f)

                val res_f = if (tool.IEEE_FP32) tool.Int2FP(res) else new FP16().int16tofloat(res)

                val rerr_tmp = (if (tool.IEEE_FP32) (tool.Int2FP(res) - tool.Int2FP(std_res)) / tool.Int2FP(std_res) else (new FP16().int16tofloat(res) - new FP16().int16tofloat(std_res)) / new FP16().int16tofloat(std_res)).abs
                val rerr = if (rerr_tmp.isNaN || rerr_tmp.isInfinity) 0.0f else rerr_tmp
                val iseq = (res == std_res) || (tool.isNan(res) && tool.isNan(std_res))
                val s_failed = !iseq
                if (!iseq) {
                  println("Something is going wrong...")
                  println(s"$std_res_f != $res_f")
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
