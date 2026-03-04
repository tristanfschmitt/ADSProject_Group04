package core_tile

import chisel3._
import chisel3.util._

// -----------------------------------------
// BTB Unit
// -----------------------------------------

class BTB extends Module {
  val io = IO(new Bundle {
    val PC = Input(UInt(32.W))
    val update = Input(Bool())
    val updatePC = Input(UInt(32.W))
    val updateTarget = Input(UInt(32.W))
    val taken = Input(Bool())

    val valid = Output(Bool())
    val target = Output(UInt(32.W))
    val predictTaken = Output(Bool())
  })

  io.valid := 0.B
  io.target := 0.U
  io.predictTaken := 0.B

  val BTB_mem = Mem(8, UInt(128.W))

  val w_index = io.updatePC(2, 0)

  val w_tag = io.updatePC(31, 3)

  val w_line = BTB_mem(w_index)

  val w_way1 = w_line(63, 0)
  val w_way2 = w_line(127, 64)

  val w_valid1 = w_way1(63)
  val w_valid2 = w_way2(63)

  val w_tag1 = w_way1(62, 34)
  val w_tag2 = w_way2(62, 34)

  val w_branchTargetAddr1 = w_way1(33, 2)
  val w_branchTargetAddr2 = w_way2(33, 2)

  val w_predictBits1 = w_way1(1, 0)
  val w_predictBits2 = w_way2(1, 0)

  when(io.update) {
    when(w_tag === w_tag1 && w_valid1) {

      val counter = w_predictBits1
      val newCounter = Wire(UInt(2.W))

      when(io.taken) {
        when(counter === 3.U) {
          newCounter := 3.U
        }.otherwise {
          newCounter := counter + 1.U
        }
      }.otherwise {
        when(counter === 0.U) {
          newCounter := 0.U
        }.otherwise {
          newCounter := counter - 1.U
        }
      }

      BTB_mem(w_index) := Cat(w_line(127, 2), newCounter)

    }.elsewhen(w_tag === w_tag2 && w_valid2) {

      val counter = w_predictBits2
      val newCounter = Wire(UInt(2.W))

      when(io.taken) {
        when(counter === 3.U) {
          newCounter := 3.U
        }.otherwise {
          newCounter := counter + 1.U
        }
      }.otherwise {
        when(counter === 0.U) {
          newCounter := 0.U
        }.otherwise {
          newCounter := counter - 1.U
        }
      }

      BTB_mem(w_index) := Cat(w_line(127, 66), newCounter, w_line(63, 0))

    }.otherwise {
      when(!w_valid1) {
        BTB_mem(w_index) := Cat(w_line(127, 64), 1.U, w_tag, io.updateTarget, 0.U, 0.U)
      }.elsewhen(!w_valid2) {
        BTB_mem(w_index) := Cat(1.U, w_tag, io.updateTarget, 0.U, 0.U, w_line(63, 0))
      }.otherwise {
        BTB_mem(w_index) := Cat(w_line(127, 64), 1.U, w_tag, io.updateTarget, 0.U, 0.U)
      }
    }
  }

  val r_index = io.PC(2, 0)

  val r_tag = io.PC(31, 3)

  val r_line = BTB_mem(r_index)

  val r_way1 = r_line(63, 0)
  val r_way2 = r_line(127, 64)

  val r_valid1 = r_way1(63)
  val r_valid2 = r_way2(63)

  val r_tag1 = r_way1(62, 34)
  val r_tag2 = r_way2(62, 34)

  val r_branchTargetAddr1 = r_way1(33, 2)
  val r_branchTargetAddr2 = r_way2(33, 2)

  val r_predictBits1 = r_way1(1, 0)
  val r_predictBits2 = r_way2(1, 0)

  when(r_tag === r_tag1 && r_valid1) {
    io.target := r_branchTargetAddr1

    when(r_predictBits1 === "b00".U) {
      io.predictTaken := 0.B
    }.elsewhen(r_predictBits1 === "b01".U) {
      io.predictTaken := 0.B
    }.elsewhen(r_predictBits1 === "b10".U) {
      io.predictTaken := 1.B
    }.elsewhen(r_predictBits1 === "b11".U) {
      io.predictTaken := 1.B
    }

    io.valid := 1.B
  }.elsewhen(r_tag === r_tag2 && r_valid2) {
    io.target := r_branchTargetAddr2

    when(r_predictBits2 === "b00".U) {
      io.predictTaken := 0.B
    }.elsewhen(r_predictBits2 === "b01".U) {
      io.predictTaken := 0.B
    }.elsewhen(r_predictBits2 === "b10".U) {
      io.predictTaken := 1.B
    }.elsewhen(r_predictBits2 === "b11".U) {
      io.predictTaken := 1.B
    }

    io.valid := 1.B
  }.otherwise {
    io.valid := 0.B
  }

}
