package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

class BitSliceMultiplierTest extends AnyFunSuite {

  test(" BitSliceMultiplierTest random test") {
    SIMCFG().compile {
      val dut = BitSliceMultiplier(multiplicandWidth = 8, multiplierWidth = 8, sliceWidth = 2, wallaceTree = false)
      dut.multiplicand_slices.simPublic()
      dut.partial_products.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)

        def monitor() = {
          val testThread = fork {
            val gen = new Random()
            val err = Array.tabulate(1 << 20)({ i =>
              dut.io.multiplier.randomize()
              dut.io.multiplicand.randomize()

              dut.clockDomain.waitSampling()
              val o = dut.io.product.toInt
              val std_o = dut.io.multiplier.toInt * dut.io.multiplicand.toInt
              if (o != std_o){
                println(s"${dut.io.multiplier.toInt} * ${dut.io.multiplicand.toInt} = ${std_o}")
                println(s"ours: ${o}")
              }
              o == std_o
            }).map { i => if (i) 0 else 1 }.sum

            if (err == 0) {
              simSuccess()
            } else {
              simFailure(s"$err errors found!!!")
            }
          }
        }

        monitor()
    }
  }

}