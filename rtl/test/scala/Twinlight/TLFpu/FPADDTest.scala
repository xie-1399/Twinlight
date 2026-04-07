package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

class FPADDTest extends AnyFunSuite {
  val tool = BasicFloatTools(fp32 = false, fp16 = true)

  Array[Boolean](true, false).foreach { outputReg =>
    test(s"Fadd32 (outputReg ${if(outputReg) "en" else "dis"}able) random test") {
      SIMCFG().compile {
        val dut = if (tool.IEEE_FP32) {
          FPU_ADD(expWidth = 8, precision = 23, outputReg = outputReg)
        } else {
          FPU_ADD(expWidth = 5, precision = 10, outputReg = outputReg)
        }
        dut
      }.doSimUntilVoid {
        dut =>
          dut.clockDomain.forkStimulus(10)

          //        SimTimeout(100000000 * 10)
          def monitor() = {
            val testThread = fork {
              val testCase = 1 << 20
              val latency = if (outputReg) 2 else 1
              var err = 0

              val ans_queue = mutable.Queue[(BigInt, BigInt, Float, Float, Int, Float)]()

              val load_thread = fork {
                for (_ <- 0 until testCase) {
                  val (a, fa) = tool.genRand()
                  val (b, fb) = tool.genRand()
                  //                val (a, fa) = (BigInt("0", 16).mod(1L << 16), new FP16().int16tofloat(BigInt("0", 16).toInt))
                  //                val (b, fb) = (BigInt("1000", 16).mod(1L << 16), new FP16().int16tofloat(BigInt("1000", 16).toInt))
                  dut.io.a #= a
                  dut.io.b #= b
                  dut.io.rm #= RoundingEncoding.RNE
                  val std_res = if (tool.IEEE_FP32) tool.FP2Int(fa + fb) else new FP16().float2int16(fa + fb)
                  //              println(s"fa + fb = ${fa + fb}")
                  val std_res_f = if (tool.IEEE_FP32) tool.Int2FP(std_res) else new FP16().int16tofloat(std_res)
                  ans_queue.enqueue((a, b, fa, fb, std_res, std_res_f))
                  dut.clockDomain.waitSampling(1)
                }
              }

              dut.clockDomain.waitSampling(latency)
              val check_thread = fork {
                for (_ <- 0 until testCase) {
                  dut.clockDomain.waitSampling(1)
                  val res = dut.io.result.toInt
                  val res_f = if (tool.IEEE_FP32) tool.Int2FP(res) else new FP16().int16tofloat(res)
                  val std = ans_queue.dequeue()
                  val std_res = std._5
                  val std_res_f = std._6
                  val fa = std._3
                  val fb = std._4
                  val a = std._1
                  val b = std._2

                  val iseq = (res == std_res) || (tool.isNan(res) && tool.isNan(std_res))
                  if (!iseq) {
                    println(s"a + b = $fa + $fb = 0x${a.toInt.toHexString} + 0x${b.toInt.toHexString} = $std_res_f = 0x${std_res.toHexString}")
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
