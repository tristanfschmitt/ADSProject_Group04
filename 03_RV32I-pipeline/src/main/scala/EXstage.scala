// ADS I Class Project
// Pipelined RISC-V Core - EX Stage
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
Instruction Execute (EX) Stage: ALU operations and exception detection

Instantiated Modules:
    ALU: Integrate your module from Assignment02 for arithmetic/logical operations

ALU Interface:
    alu.io.operandA: first operand input
    alu.io.operandB: second operand input
    alu.io.operation: operation code controlling ALU function
    alu.io.aluResult: computation result output

Internal Signals:
    Map uopc codes to ALUOp values

Functionality:
    Map instruction uop to ALU operation code
    Pass operands to ALU
    Output results to pipeline

Outputs:
    aluResult: computation result from ALU
    exception: pass exception flag
*/

package core_tile

import chisel3._
import chisel3.util._
import Assignment02.{ALU, ALUOp}
import uopc._

// -----------------------------------------
// Execute Stage
// -----------------------------------------

class EXstage extends Module {
  val io = IO(new Bundle {
    val inUOP = Input(uopc())
    val inOperandA = Input(UInt(32.W))
    val inOperandB = Input(UInt(32.W))
    val inXcptInvalid = Input(Bool())

    val RsAddr = Input(UInt(5.W))
    val RtAddr = Input(UInt(5.W))

    val rdEX = Input(UInt(5.W))
    val rdMEM = Input(UInt(5.W))
    val rdWB = Input(UInt(5.W))

    val aluResEX = Input(UInt(32.W))
    val aluResMEM = Input(UInt(32.W))
    val aluResWB = Input(UInt(32.W))

    val inBranchDest = Input(UInt(32.W))
    val branchPC = Input(UInt(32.W))

    val outPCnew = Output(UInt(32.W))

    val aluResult = Output(UInt(32.W))
    val exception = Output(Bool())
    val outFlush = Output(Bool())

    val update = Output(Bool())
    val updatePC = Output(UInt(32.W))
    val updateTarget = Output(UInt(32.W))
    val taken = Output(Bool())
  })

  val alu = Module(new ALU)

  val branchTaken = WireDefault(false.B)
  val aluOp = WireDefault(ALUOp.PASSB)
  val validOp = WireDefault(false.B)

  io.outFlush := false.B
  io.outPCnew := 0.U

  io.aluResult := 0.U

  io.update := 0.B
  io.taken := branchTaken
  io.updateTarget := io.inBranchDest
  io.updatePC := io.branchPC

  when(io.RsAddr =/= 0.U && io.RsAddr === io.rdEX) {
    alu.io.operandA := io.aluResEX
  }.elsewhen(io.RsAddr =/= 0.U && io.RsAddr === io.rdMEM) {
    alu.io.operandA := io.aluResMEM
  }.elsewhen(io.RsAddr =/= 0.U && io.RsAddr === io.rdWB) {
    alu.io.operandA := io.aluResWB
  }.otherwise {
    alu.io.operandA := io.inOperandA
  }

  when(io.RtAddr =/= 0.U && io.RtAddr === io.rdEX) {
    alu.io.operandB := io.aluResEX
  }.elsewhen(io.RtAddr =/= 0.U && io.RtAddr === io.rdMEM) {
    alu.io.operandB := io.aluResMEM
  }.elsewhen(io.RtAddr =/= 0.U && io.RtAddr === io.rdWB) {
    alu.io.operandB := io.aluResWB
  }.otherwise {
    alu.io.operandB := io.inOperandB
  }

  when(!io.inXcptInvalid) {

    switch(io.inUOP) {

      is(uopc.NOP) {
        validOp := true.B
      }
      is(uopc.ADD, uopc.ADDI) {
        aluOp := ALUOp.ADD
        validOp := true.B
      }
      is(uopc.SUB) {
        aluOp := ALUOp.SUB
        validOp := true.B
      }
      is(uopc.AND, uopc.ANDI) {
        aluOp := ALUOp.AND
        validOp := true.B
      }
      is(uopc.OR, uopc.ORI) {
        aluOp := ALUOp.OR
        validOp := true.B
      }
      is(uopc.XOR, uopc.XORI) {
        aluOp := ALUOp.XOR
        validOp := true.B
      }
      is(uopc.SLL, uopc.SLLI) {
        aluOp := ALUOp.SLL
        validOp := true.B
      }
      is(uopc.SRL, uopc.SRLI) {
        aluOp := ALUOp.SRL
        validOp := true.B
      }
      is(uopc.SRA, uopc.SRAI) {
        aluOp := ALUOp.SRA
        validOp := true.B
      }
      is(uopc.SLT, uopc.SLTI) {
        aluOp := ALUOp.SLT
        validOp := true.B
      }
      is(uopc.SLTU, uopc.SLTIU) {
        aluOp := ALUOp.SLTU
        validOp := true.B
      }
      is(uopc.BEQ, uopc.BNE) {
        aluOp := ALUOp.SUB
        validOp := true.B
      }
      is(uopc.BLT, uopc.BGE) {
        aluOp := ALUOp.SLT
        validOp := true.B
      }
      is(uopc.BLTU, uopc.BGEU) {
        aluOp := ALUOp.SLTU
        validOp := true.B
      }
    }
  }

  io.exception := io.inXcptInvalid || !validOp

  alu.io.operation := aluOp

  when(!io.inXcptInvalid) {
    switch(io.inUOP) {
      is(uopc.BEQ) {
        branchTaken := alu.io.aluResult === 0.U
        io.update := 1.B
      }
      is(uopc.BNE) {
        branchTaken := alu.io.aluResult =/= 0.U
        io.update := 1.B
      }
      is(uopc.BLT) {
        branchTaken := alu.io.aluResult === 1.U
        io.update := 1.B
      }
      is(uopc.BGE) {
        branchTaken := alu.io.aluResult === 0.U
        io.update := 1.B
      }
      is(uopc.BLTU) {
        branchTaken := alu.io.aluResult === 1.U
        io.update := 1.B
      }
      is(uopc.BGEU) {
        branchTaken := alu.io.aluResult === 0.U
        io.update := 1.B
      }
    }
  }

  io.aluResult := alu.io.aluResult

  when(branchTaken && validOp && !io.inXcptInvalid) {
    io.outFlush := true.B
    io.outPCnew := io.inBranchDest
  }

}