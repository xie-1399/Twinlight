package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

class BitSliceMultiplierTest extends AnyFunSuite {

  val tool = BasicFloatTools(fp32 = false, fp16 = true)
  val pipelineStage = Seq(1)
  val mulLatency = pipelineStage.size

  Seq(true, false).foreach { wallaceTree =>
    test(s"BitSliceMultiplierTest (wallaceTree ${if (wallaceTree) "en" else "dis"}able) random test") {
      SIMCFG().compile {
        val dut = BitSliceMultiplier(multiplicandWidth = 8, multiplierWidth = 8, sliceWidth = 2, wallaceTree = wallaceTree, pipeAt = pipelineStage)
        dut.multiplicand_slices.simPublic()
        dut.partial_products.simPublic()
        dut
      }.doSimUntilVoid {
        dut =>
          dut.clockDomain.forkStimulus(10)

          def monitor() = {
            val testThread = fork {
              val ans_queue = mutable.Queue[(Int, Int, Int)]()
              val latency = mulLatency
              val testCase = 1 << 20
              var err = 0

              val load_thread = fork {
                for (_ <- 0 until testCase) {
                  val a = dut.io.multiplier.randomize()
                  val b = dut.io.multiplicand.randomize()
                  val std_res = (a * b).toInt
                  ans_queue.enqueue((a.toInt, b.toInt, std_res))
                  dut.clockDomain.waitSampling(1)
                }
              }
              dut.clockDomain.waitSampling(latency)
              val check_thread = fork {
                for (_ <- 0 until testCase) {
                  dut.clockDomain.waitSampling(1)
                  val res = dut.io.product.toInt
                  val std = ans_queue.dequeue()
                  val std_res = std._3
                  val a = std._1
                  val b = std._2
                  val iseq = res == std_res
                  if (!iseq) {
                    println(s"${a} * ${b} = ${std_res}")
                    println(s"ours: ${res}")
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