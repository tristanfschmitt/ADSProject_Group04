// ADS I Class Project
// Pipelined RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/15/2023 by Tobias Jauch (@tojauch)

/*
The goal of this task is to implement a 5-stage pipeline that features a subset of RV32I (all R-type and I-type instructions). 

    Instruction Memory:
        The CPU has an instruction memory (IMem) with 4096 words, each of 32 bits.
        The content of IMem is loaded from a binary file specified during the instantiation of the MultiCycleRV32Icore module.

    CPU Registers:
        The CPU has a program counter (PC) and a register file (regFile) with 32 registers, each holding a 32-bit value.
        Register x0 is hard-wired to zero.

    Microarchitectural Registers / Wires:
        Various signals are defined as either registers or wires depending on whether they need to be used in the same cycle or in a later cycle.

    Processor Stages:
        The FSM of the processor has five stages: fetch, decode, execute, memory, and writeback.
        All stages are active at the same time and process different instructions simultaneously.

        Fetch Stage:
            The instruction is fetched from the instruction memory based on the current value of the program counter (PC).

        Decode Stage:
            Instruction fields such as opcode, rd, funct3, and rs1 are extracted.
            For R-type instructions, additional fields like funct7 and rs2 are extracted.
            Control signals (isADD, isSUB, etc.) are set based on the opcode and funct3 values.
            Operands (operandA and operandB) are determined based on the instruction type.

        Execute Stage:
            Arithmetic and logic operations are performed based on the control signals and operands.
            The result is stored in the aluResult register.

        Memory Stage:
            No memory operations are implemented in this basic CPU.

        Writeback Stage:
            The result of the operation (writeBackData) is written back to the destination register (rd) in the register file.

    Check Result:
        The final result (writeBackData) is output to the io.check_res signal.
        The exception signal is also passed to the wrapper module. It indicates whether an invalid instruction has been encountered.
        In the fetch stage, a default value of 0 is assigned to io.check_res.
*/

package core_tile

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import Assignment02.{ALU, ALUOp}
import uopc._


class PipelinedRV32Icore (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val check_res = Output(UInt(32.W))
    val exception = Output(Bool())
  })

  val ifStage = Module(new IFstage(BinaryFile))
  val idStage = Module(new IDstage)
  val exStage = Module(new EXstage)
  val memStage = Module(new MEMstage)
  val wbStage = Module(new WBstage)

  val ifBarrier = Module(new IFbarrier)
  val idBarrier = Module(new IDbarrier)
  val exBarrier = Module(new EXbarrier)
  val memBarrier = Module(new MEMbarrier)
  val wbBarrier = Module(new WBbarrier)

  ifBarrier.io.inInstr := ifStage.io.instr

  idStage.io.inInstr := ifBarrier.io.outInstr

  idBarrier.io.inUOP := idStage.io.uop
  idBarrier.io.inRD := idStage.io.rd
  idBarrier.io.inOperandA := idStage.io.operandA
  idBarrier.io.inOperandB := idStage.io.operandB
  idBarrier.io.inXcptInvalid := idStage.io.XcptInvalid

  exStage.io.inUOP := idBarrier.io.outUOP
  exStage.io.inOperandA := idBarrier.io.outOperandA
  exStage.io.inOperandB := idBarrier.io.outOperandB
  exStage.io.inXcptInvalid := idBarrier.io.outXcptInvalid

  exBarrier.io.inaluResult := exStage.io.aluResult
  exBarrier.io.inRD := idBarrier.io.outRD
  exBarrier.io.inXcptInvalid := exStage.io.exception

  memStage.io.inAluResult := exBarrier.io.outAluResult
  memStage.io.inException := exBarrier.io.outXcptInvalid

  memBarrier.io.inAluResult := memStage.io.outAluResult
  memBarrier.io.inRD := exBarrier.io.outRD
  memBarrier.io.inException := memStage.io.outException

  wbStage.io.aluResult := memBarrier.io.outAluResult
  wbStage.io.rd := memBarrier.io.outRD
  wbStage.io.exception := memBarrier.io.outException

  idStage.io.inWr_en := wbStage.io.outWr_en
  idStage.io.inRd := wbStage.io.outRd
  idStage.io.inWd := wbStage.io.outData

  wbBarrier.io.inCheckRes := wbStage.io.check_res
  wbBarrier.io.inXcptInvalid := memBarrier.io.outException

  io.check_res := wbBarrier.io.inCheckRes
  io.exception := wbBarrier.io.inXcptInvalid

  wbBarrier.io.inRD := wbStage.io.outRd

  //Forwarding ---------------------------------------------------------------------

  idBarrier.io.inRsAddr := idStage.io.outRsAddr
  idBarrier.io.inRtAddr := idStage.io.outRtAddr

  exStage.io.rdEX := exBarrier.io.outRD
  exStage.io.rdMEM := memBarrier.io.outRD
  exStage.io.rdWB := wbBarrier.io.outRD

  exStage.io.aluResEX := exBarrier.io.outAluResult
  exStage.io.aluResMEM := memBarrier.io.outAluResult
  exStage.io.aluResWB := wbBarrier.io.outCheckRes

  exStage.io.RsAddr := idBarrier.io.outRsAddr
  exStage.io.RtAddr := idBarrier.io.outRtAddr

}
