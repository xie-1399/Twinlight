package Twinlight.TLUtils

import Twinlight.TLFpu._
import Twinlight.TLUtils.TLPlugin.TLModule
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class PingPongBuffer(depth: Int, width: Int) extends TLModule {
  val io = new Bundle {
    val waddr = in port UInt(log2Up(depth) bits)
    val wvalid = in port Bool()
    val wdata = in port Bits(width bits)
    val wdone = in port Bool()

    val raddr = in port UInt(log2Up(depth) bits)
    val rdata = out port Bits(width bits)
    val rdone = in port Bool()
  }

  val regs = Vec.fill(2)(Vec.fill(depth)(Reg(Bits(width bits))))
  val reg_wsel = Reg(UInt(1 bits))
  val reg_rsel = Reg(UInt(1 bits))

  val r_done = Reg(Bool())
  val w_done = Reg(Bool())

  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val WA_RB = new State
    val WB_RA = new State

    IDLE.whenIsActive {
      r_done := True
      w_done := False
      reg_wsel := U(0, 1 bits)
      reg_rsel := U(1, 1 bits)
      goto(WA_RB)
    }

    WA_RB.whenIsActive {
      when(io.rdone) {
        r_done := True
      }
      when(io.wdone){
        w_done := True
      }
      when((w_done || io.wdone) && (r_done || io.rdone)) {
        goto(WB_RA)
      }
    }
    WA_RB.onExit {
      r_done := False
      w_done := False
      reg_wsel := U(1, 1 bits)
      reg_rsel := U(0, 1 bits)
    }

    WB_RA.whenIsActive {
      when(io.rdone) {
        r_done := True
      }
      when(io.wdone){
        w_done := True
      }
      when((w_done || io.wdone) && (r_done || io.rdone)) {
        goto(WA_RB)
      }
    }
    WB_RA.onExit {
      r_done := False
      w_done := False
      reg_wsel := U(0, 1 bits)
      reg_rsel := U(1, 1 bits)
    }

  }
  when(io.wvalid){
    regs(reg_wsel)(io.waddr) := io.wdata
  }

  io.rdata := regs(reg_rsel)(io.raddr)
}