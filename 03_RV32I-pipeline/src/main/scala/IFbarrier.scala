// ADS I Class Project
// Pipelined RISC-V Core - IF Barrier
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
IF-Barrier: pipeline register between Fetch and Decode stages

Internal Registers:
    instrReg: holds instruction between pipeline stages, initialized to 0

Inputs:
    inInstr: fetched instruction from IF stage

Outputs:
    outInstr: instruction to ID stage

Functionality:
    Save all input signals to a register and output them in the following clock cycle
*/

package core_tile

import chisel3._

// -----------------------------------------
// IF-Barrier
// -----------------------------------------

class IFbarrier extends Module {
  val io = IO(new Bundle {
    val inInstr = Input(UInt(32.W))
    val inPC = Input(UInt(32.W))
    val inPredTaken = Input(Bool())
    val outPC = Output(UInt(32.W))
    val outInstr = Output(UInt(32.W))
    val outPredTaken = Output(Bool())
    val inFlush = Input(Bool())
  })

  val instrReg = RegInit(0.U(32.W))
  val pcReg = RegInit(0.U(32.W))
  val predTakenReg = RegInit(false.B)

  instrReg := io.inInstr
  pcReg := io.inPC
  predTakenReg := io.inPredTaken

  when(io.inFlush) {
    io.outInstr := "h00000013".U
    io.outPredTaken := false.B
  }.otherwise {
    io.outInstr := instrReg
    io.outPredTaken := predTakenReg
  }

  io.outPC := pcReg

}
