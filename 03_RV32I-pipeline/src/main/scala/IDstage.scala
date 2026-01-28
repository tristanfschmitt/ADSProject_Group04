// ADS I Class Project
// Pipelined RISC-V Core - ID Stage
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
Instruction Decode (ID) Stage: decoding and operand fetch

Extracted Fields from 32-bit Instruction (see RISC-V specification for reference):
    opcode: instruction format identifier
    funct3: selects variant within instruction format
    funct7: further specifies operation type (R-type only)
    rd: destination register address
    rs1: first source register address
    rs2: second source register address
    imm: 12-bit immediate value (I-type, sign-extended)

Register File Interfaces:
    regFileReq_A, regFileResp_A: read port for rs1 operand
    regFileReq_B, regFileResp_B: read port for rs2 operand

Internal Signals:
    Combinational decoders for instructions

Functionality:
    Decode opcode to determine instruction and identify operation (ADD, SUB, XOR, ...)
    Output: uop (operation code), rd, operandA (from rs1), operandB (rs2 or immediate)

Outputs:
    uop: micro-operation code (identifies instruction type)
    rd: destination register index
    operandA: first operand
    operandB: second operand 
    XcptInvalid: exception flag for invalid instructions
*/

package core_tile

import chisel3._
import chisel3.util._
import uopc._

// -----------------------------------------
// Decode Stage
// -----------------------------------------

class IDstage extends Module {
  val io = IO(new Bundle {
    val inInstr = Input(UInt(32.W))
    val uop = Output(uopc())
    val rd = Output(UInt(5.W))
    val operandA = Output(UInt(32.W))
    val operandB = Output(UInt(32.W))
    val XcptInvalid = Output(Bool())
  })

  val regFile = Module(new RegisterFile)

  io.rd := io.inInstr(11, 7)

  regFile.io.req_1.addr := io.inInstr(19, 15)
  regFile.io.req_2.addr := io.inInstr(24, 20)

  io.operandA := regFile.io.resp_1.data

  val opcode = io.inInstr(6, 0)
  val funct3 = io.inInstr(14, 12)
  val funct7 = io.inInstr(31, 25)

  val uopReg = WireDefault(uopc.NOP)
  val xcptInvalid = WireDefault(false.B)

  switch(opcode) {
    is("b0110011".U) { // R-Type
      switch(funct3) {
        is("b000".U) {
          when(funct7 === "b0000000".U) { uopReg := uopc.ADD }
            .elsewhen(funct7 === "b0100000".U) { uopReg := uopc.SUB }
            .otherwise { xcptInvalid := true.B }
        }
        is("b001".U) {
          when(funct7 === "b0000000".U) { uopReg := uopc.SLL }
            .otherwise { xcptInvalid := true.B }
        }
        is("b010".U) {
          when(funct7 === "b0000000".U) { uopReg := uopc.SLT }
            .otherwise { xcptInvalid := true.B }
        }
        is("b011".U) {
          when(funct7 === "b0000000".U) { uopReg := uopc.SLTU }
            .otherwise { xcptInvalid := true.B }
        }
        is("b100".U) {
          when(funct7 === "b0000000".U) { uopReg := uopc.XOR }
            .otherwise { xcptInvalid := true.B }
        }
        is("b101".U) {
          when(funct7 === "b0000000".U) { uopReg := uopc.SRL }
            .elsewhen(funct7 === "b0100000".U) { uopReg := uopc.SRA }
            .otherwise { xcptInvalid := true.B }
        }
        is("b110".U) {
          when(funct7 === "b0000000".U) { uopReg := uopc.OR }
            .otherwise { xcptInvalid := true.B }
        }
        is("b111".U) {
          when(funct7 === "b0000000".U) { uopReg := uopc.AND }
            .otherwise { xcptInvalid := true.B }
        }
        otherwise { xcptInvalid := true.B }
      }
    }

    is("b0010011".U) { // I-Type ALU
      switch(funct3) {
        is("b000".U) { uopReg := uopc.ADDI }
        is("b010".U) { uopReg := uopc.SLTI }
        is("b011".U) { uopReg := uopc.SLTIU }
        is("b100".U) { uopReg := uopc.XORI }
        is("b110".U) { uopReg := uopc.ORI  }
        is("b111".U) { uopReg := uopc.ANDI }
        is("b001".U) {
          when(funct7 === "b0000000".U) { uopReg := uopc.SLLI }
            .otherwise { xcptInvalid := true.B }
        }
        is("b101".U) {
          when(funct7 === "b0000000".U) { uopReg := uopc.SRLI }
            .elsewhen(funct7 === "b0100000".U) { uopReg := uopc.SRAI }
            .otherwise { xcptInvalid := true.B }
        }
        otherwise { xcptInvalid := true.B }
      }
    }

    otherwise {
      xcptInvalid := true.B
    }
  }

  io.uop := uopReg
  io.XcptInvalid := xcptInvalid

  when(opcode === "b0110011".U) { //R-Type
    io.operandB := regFile.io.resp_2.data
  }.elsewhen(opcode === "b0010011".U) { //I-Type
    io.operandB := Cat(Fill(20, io.inInstr(31)), io.inInstr(31,20))
  }.otherwise {
    io.operandB := 0.U
  }

}