// ADS I Class Project
// Pipelined RISC-V Core - IF Stage
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
The Instruction Fetch (IF) stage is the first stage of the pipeline and handles instruction retrieval from memory.

Memory:
    IMem: instruction memory with 4096 32-bit unsigned integer entires, loaded from a binary file at compile time

Internal Registers:
    PC: 32-bit unsigned integer register, initialized to 0 holding the current program counter address

Internal Signals:
    none

Functionality:
    Fetch the instruction at the current PC (word-aligned addressing)
    Increment the PC (word-aligned) each clock cycle to fetch the next sequential instruction

Parameters:
    BinaryFile: String - path to the binary file to load into instruction memory

Inputs:
    none

Outputs:
    instr: send the fetched instruction to IF Barrier
*/

package core_tile

import chisel3._
import chisel3.util.experimental.loadMemoryFromFile

// -----------------------------------------
// Fetch Stage
// -----------------------------------------

class IFstage(BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val inPCNew = Input(UInt(32.W))
    val inPCNewEx = Input(UInt(32.W))
    val inPCSrc = Input(Bool())
    val inPCSrcEx = Input(Bool())

    val valid = Input(Bool())
    val target = Input(UInt(32.W))
    val predictTaken = Input(Bool())

    val instr = Output(UInt(32.W))
    val outPC = Output(UInt(32.W))
    val PC = Output(UInt(32.W))
    val predTaken = Output(Bool())
  })

  val PC = RegInit(0.U(32.W))
  val IMem = Mem(4096, UInt(32.W))

  loadMemoryFromFile(IMem, BinaryFile)

  io.PC := PC

  io.predTaken := 0.B

  val instrPC = Wire(UInt(32.W))
  val nextPC = Wire(UInt(32.W))
  instrPC := PC
  nextPC := PC + 1.U

  when(io.inPCSrcEx) {
    instrPC := io.inPCNewEx
    nextPC := io.inPCNewEx + 1.U
  }.elsewhen(io.inPCSrc) {
    instrPC := io.inPCNew
    nextPC := io.inPCNew + 1.U
  }.elsewhen(io.valid && io.predictTaken) {
    nextPC := io.target
    io.predTaken := 1.B
  }

  io.instr := IMem(instrPC)
  io.outPC := instrPC

  PC := nextPC
}
