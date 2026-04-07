package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

class MultiplierTest extends AnyFunSuite {
  val tool = BasicFloatTools(fp32 = false, fp16 = true)
  val pipelineStage = Seq(1) // leq than 2
  val mulLatency = pipelineStage.size

  test("MultiplierTest random test") {
    SIMCFG().compile {
      val dut = if (tool.IEEE_FP32) {
        Multiplier(23 + 1 + 1, pipelineStage)
      } else {
        Multiplier(10 + 1 + 1, pipelineStage)
      }
      dut.columns.foreach(_.foreach(_.simPublic()))
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)

        def monitor() = {
          val testThread = fork {
            val testCase = 1 << 20
            val latency = mulLatency
            val ans_queue = mutable.Queue[(Int, Int, Int)]()
            var err = 0

            val load_thread = fork {
              for (_ <- 0 until testCase) {
                val a = tool.gen.nextInt(1 << 11)
                val b = tool.gen.nextInt(1 << 11)
                val std_res = a * b
                dut.io.a #= a
                dut.io.b #= b
                dut.io.regEnables.foreach(_ #= true)
                ans_queue.enqueue((a, b, std_res))
                dut.clockDomain.waitSampling(1)
              }
            }
            dut.clockDomain.waitSampling(latency)
            val check_thread = fork {
              for(_ <- 0 until testCase){
                dut.clockDomain.waitSampling(1)
                val res = dut.io.result.toInt
                val std = ans_queue.dequeue()
                val std_res = std._3
                val a = std._1
                val b = std._2
                val iseq = res == std_res
                if (!iseq) {
                  println(s"a * b = $a * $b = 0x${a.toHexString} * 0x${b.toHexString} = $std_res = 0x${std_res.toHexString}")
                  println(s"res     ${"".padTo((a.toString.length + b.toString.length + 12 + a.toString().length + b.toString().length), ' ')} = $res = 0x${res.toHexString}")
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
