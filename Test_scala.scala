package MAC

import chisel3._
import chisel3.util._

import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._

class MyMAC(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new MyMACModuleImp(this)
}

class MyMACModuleImp(outer: MyMAC)(implicit p: Parameters)
  extends LazyRoCCModuleImp(outer)
  with HasCoreParameters {

  val acc = RegInit(0.S((2 * xLen).W))
  val count = RegInit(0.U(5.W))

  val respValid = RegInit(false.B)
  val respData  = RegInit(0.U(xLen.W))
  val respRd    = RegInit(0.U(5.W))

  io.cmd.ready := !respValid

  io.resp.valid := respValid
  io.resp.bits.data := respData
  io.resp.bits.rd := respRd

  when (io.resp.fire()) {
    respValid := false.B
  }

  io.busy := respValid
  io.interrupt := false.B

  io.mem.req.valid := false.B
  io.mem.req.bits := DontCare

  when (io.cmd.fire()) {
    val funct = io.cmd.bits.inst.funct
    val rs1 = io.cmd.bits.rs1.asSInt
    val rs2 = io.cmd.bits.rs2.asSInt

    when (funct === 0.U) {
      when (count < 25.U) {
        acc := acc + (rs1 * rs2)
        count := count + 1.U
      }

    }.elsewhen (funct === 1.U) {
      respData := acc.asUInt()(xLen - 1, 0)
      respRd := io.cmd.bits.inst.rd
      respValid := true.B

      acc := 0.S
      count := 0.U

    }.elsewhen (funct === 2.U) {
      acc := 0.S
      count := 0.U
      respData := 0.U
      respRd := 0.U
      respValid := false.B
    }
  }
}
