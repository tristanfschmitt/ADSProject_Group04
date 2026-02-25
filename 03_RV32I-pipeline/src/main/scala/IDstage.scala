package core_tile

import chisel3._
import chisel3.util._
import uopc._

class IDstage extends Module {
  val io = IO(new Bundle {
    val inInstr = Input(UInt(32.W))
    val inRd = Input(UInt(5.W))
    val inWd = Input(UInt(32.W))
    val inWr_en = Input(Bool())
    val inPC = Input(UInt(32.W))

    val uop = Output(uopc())
    val rd = Output(UInt(5.W))
    val operandA = Output(UInt(32.W))
    val operandB = Output(UInt(32.W))
    val XcptInvalid = Output(Bool())
    val outPC = Output(UInt(32.W))
    val outPCSrc = Output(Bool())

    val outRsAddr = Output(UInt(5.W))
    val outRtAddr = Output(UInt(5.W))
  })

  val regFile = Module(new regFile)

  io.outPCSrc := false.B
  io.outPC := 0.U

  io.rd := io.inInstr(11, 7)

  regFile.io.req_1.addr := io.inInstr(19, 15)
  regFile.io.req_2.addr := io.inInstr(24, 20)

  io.outRsAddr := 0.U
  io.outRtAddr := 0.U

  regFile.io.req_3.wr_en := false.B
  regFile.io.req_3.addr := 0.U
  regFile.io.req_3.data := 0.U
  when(io.inWr_en) {
    regFile.io.req_3.wr_en := true.B
    regFile.io.req_3.addr := io.inRd
    regFile.io.req_3.data := io.inWd
  }

  io.operandA := regFile.io.resp_1.data

  val opcode = io.inInstr(6, 0)
  val funct3 = io.inInstr(14, 12)
  val funct7 = io.inInstr(31, 25)

  io.uop := uopc.NOP
  io.XcptInvalid := true.B
  io.operandB := 0.U

  switch(opcode) {
    is("b0110011".U) {
      io.XcptInvalid := false.B
      io.operandB := regFile.io.resp_2.data

      io.outRsAddr := io.inInstr(19, 15)
      io.outRtAddr := io.inInstr(24, 20)

      switch(funct3) {
        is("b000".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.ADD
          }
            .elsewhen(funct7 === "b0100000".U) {
              io.uop := uopc.SUB
            }
        }
        is("b001".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.SLL
          }
        }
        is("b010".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.SLT
          }
        }
        is("b011".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.SLTU
          }
        }
        is("b100".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.XOR
          }
        }
        is("b101".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.SRL
          }
            .elsewhen(funct7 === "b0100000".U) {
              io.uop := uopc.SRA
            }
        }
        is("b110".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.OR
          }
        }
        is("b111".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.AND
          }
        }
      }
    }

    is("b0010011".U) {
      io.XcptInvalid := false.B
      io.operandB := Cat(Fill(20, io.inInstr(31)), io.inInstr(31, 20))

      io.outRsAddr := io.inInstr(19, 15)

      switch(funct3) {
        is("b000".U) {
          io.uop := uopc.ADDI
        }
        is("b010".U) {
          io.uop := uopc.SLTI
        }
        is("b011".U) {
          io.uop := uopc.SLTIU
        }
        is("b100".U) {
          io.uop := uopc.XORI
        }
        is("b110".U) {
          io.uop := uopc.ORI
        }
        is("b111".U) {
          io.uop := uopc.ANDI
        }
        is("b001".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.SLLI
          }
        }
        is("b101".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.SRLI
          }.elsewhen(funct7 === "b0100000".U) {
            io.uop := uopc.SRAI
          }
        }
      }
    }
    is("b1101111".U) {
      io.XcptInvalid := false.B
      io.rd := 0.U
      io.uop := uopc.NOP

      regFile.io.req_3.addr := io.inInstr(11, 7)
      regFile.io.req_3.data := io.inPC
      regFile.io.req_3.wr_en := true.B

      val jalImm20 = Cat(
        io.inInstr(31),          // imm[20]
        io.inInstr(19,12),       // imm[19:12]
        io.inInstr(20),          // imm[11]
        io.inInstr(30,21),       // imm[10:1]
      )

      // Sign-extend
      val jalImm = Cat(Fill(12, jalImm20(19)), jalImm20)

      io.outPC := io.inPC + jalImm + 1.U

      io.outPCSrc := true.B

    }
    is("b1100111".U) {
      switch(funct3) {
        is("b000".U) {
          io.XcptInvalid := false.B
          io.rd := 0.U
          io.uop := uopc.NOP

          regFile.io.req_3.addr := io.inInstr(11, 7)
          regFile.io.req_3.data := io.inPC
          regFile.io.req_3.wr_en := true.B

          io.outPC := Cat(Fill(20, io.inInstr(31)), io.inInstr(31, 20)) + regFile.io.resp_1.data

          io.outPCSrc := true.B
        }
      }
    }
  }
}
