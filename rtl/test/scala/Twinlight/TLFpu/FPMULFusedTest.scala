package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable
import scala.language.postfixOps

class FPMULFusedTest extends AnyFunSuite {
  val tool = BasicFloatTools(fp32 = false, fp16 = true)
  val pipeAt = Seq(1)
  val testConfigs = Array(
    (true, true),
    (true, false),
    (false, true),
    (false, false),
  )
  Seq(true, false).foreach { outputReg =>
    testConfigs.foreach { testConfig =>
      test(s"FPMULFusedTest(is_wallace = ${testConfig._1}, is_bitslice = ${testConfig._2}, outputReg ${if (outputReg) "en" else "dis"}able) random test") {
        SIMCFG().compile {
          val dut = if (tool.IEEE_FP32) {
            FMULFused(expWidth = 8, precision = 23, intWidth = 8, is_wallace = testConfig._1, is_bitslice = testConfig._2, outputReg = outputReg, pipeAt = pipeAt)
          } else {
            FMULFused(expWidth = 5, precision = 10, intWidth = 8, is_wallace = testConfig._1, is_bitslice = testConfig._2, outputReg = outputReg, pipeAt = pipeAt)
          }
          dut
        }.doSimUntilVoid {
          dut =>
            dut.clockDomain.forkStimulus(10)

            def monitor() = {
              val testThread = fork {
                val testCase = 1 << 20
                val latency = if (outputReg) 3 else 2
                val ans_queue = mutable.Queue[(BigInt, Float, BigInt, Float, Int, Float)]()
                var err = 0

                val load_thread = fork {
                  for (_ <- 0 until testCase) {
                    val (a, fa) = tool.genRand()
                    dut.io.a #= a
                    val b = dut.io.b.randomize()
                    val fb = b.toFloat
                    dut.io.rm #= RoundingEncoding.RNE
                    val std_res = if (tool.IEEE_FP32) tool.FP2Int(fa * fb) else new FP16().float2int16(fa * fb)
                    val std_res_f = if (tool.IEEE_FP32) tool.Int2FP(std_res) else new FP16().int16tofloat(std_res)
                    // println(s"fa + fb = ${fa + fb}")
                    ans_queue.enqueue((a, fa, b, fb, std_res, std_res_f))
                    dut.clockDomain.waitSampling(1)
                  }
                }

                dut.clockDomain.waitSampling(latency)

                val check_thread = fork {
                  for (_ <- 0 until testCase) {
                    dut.clockDomain.waitSampling(1)
                    val res = dut.io.result.toInt
                    val std = ans_queue.dequeue()
                    val a = std._1
                    val fa = std._2
                    val b = std._3
                    val fb = std._4
                    val std_res = std._5
                    val std_res_f = std._6
                    val res_f = if (tool.IEEE_FP32) tool.Int2FP(res) else new FP16().int16tofloat(res)
                    val rerr_tmp = (if (tool.IEEE_FP32) (tool.Int2FP(res) - tool.Int2FP(std_res)) / tool.Int2FP(std_res) else (new FP16().int16tofloat(res) - new FP16().int16tofloat(std_res)) / new FP16().int16tofloat(std_res)).abs
                    val rerr = if (rerr_tmp.isNaN || rerr_tmp.isInfinity) 0.0f else rerr_tmp
                    val iseq = (res == std_res) || (tool.isNan(res) && tool.isNan(std_res))
                    if (!iseq) {
                      println(s"a * b = $fa * $fb = 0x${a.toInt.toHexString} * 0x${b.toInt.toHexString} = $std_res_f = 0x${std_res.toHexString}")
                      println(s"res     ${"".padTo((fa.toString.length + fb.toString.length + 12 + a.toString().length + b.toString().length), ' ')} = $res_f = 0x${res.toHexString}")
                      println(" ")
                      err += 1
                    }
                  }
                }

                load_thread.join()
                check_thread.join()

                dut.clockDomain.waitSampling(100)

                if (err == 0) {
                  println(s"=====Test Passed!!!=====")
                  simSuccess()
                } else {
                  simFailure(s"=====Failed!!! $err Occurred!!!=====")
                }
              }
            }
            monitor()
        }
      }
    }
  }

}
