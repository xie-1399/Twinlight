package Twinlight.TLUtils

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.language.postfixOps

class PingPongBufferTest extends AnyFunSuite {

  test("PingPongBuffer random test") {
    SIMCFG().compile {
      val dut = PingPongBuffer(depth = 1, width = 16)
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        val testCase = 1 << 20

        def monitor() = {
          val wqueue = mutable.Queue[BigInt]()
          val rqueue = mutable.Queue[BigInt]()
          dut.io.rdone #= false
          dut.io.wdone #= false
          dut.io.wvalid #= false
          var err = 0

          dut.clockDomain.waitSampling(10) // wait the initialization

          val test = fork {
            (0 until testCase).foreach { _ =>
              dut.io.waddr #= 0
              dut.io.wdata.randomize()
              dut.io.wdone #= true
              dut.io.wvalid #= true
              dut.clockDomain.waitSampling()
              wqueue.enqueue(dut.io.wdata.toBigInt)
              dut.io.raddr #= 0
              dut.clockDomain.waitFallingEdge()
              rqueue.enqueue(dut.io.rdata.toBigInt)
              dut.io.rdone #= true
              if (wqueue.dequeue() != rqueue.dequeue()) {
                err += 1
              }
            }
          }
          dut.clockDomain.waitSampling(testCase + 10)

          if (err == 0) {
            simSuccess()
          } else {
            simFailure()
          }

        }

        monitor()
    }
  }

}
