//package chipyard
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

class MyMACModuleImp(outer: MyMAC)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
    
    val N = 32
    val GP = 7
    
    val inputBase = RegInit(0.U(xLen.W))
    val kernelBase = RegInit(0.U(xLen.W))
    val outputBase = RegInit(0.U(xLen.W))
    
    
    val row = RegInit(0.U(6.W))
    val col = RegInit(0.U(6.W))
    val groupIdx = RegInit(0.U(4.W))
    val kernelLoadIdx = RegInit(0.U(4.W))
    
    val inputWord = RegInit(0.U(xLen.W))
    val kernelBuf = RegInit(VecInit(Seq.fill(GP)(0.U(xLen.W))))
    
    val acc = RegInit(0.U(xLen.W))
    val resultReg = RegInit(0.U(xLen.W))
    
    val doneReg = RegInit(false.B)
    val busyReg = RegInit(false.B)
    val errorReg = RegInit(false.B)
    val dprvReg = RegInit(0.U(2.W))
    
    val respValid = RegInit(false.B)
    val respData = RegInit(0.U(xLen.W))
    val respRd = RegInit(0.U(5.W))
    
    val sIdle :: sLoadKernelBufReq :: sLoadKernelBufResp :: sLoadInputReq :: sLoadInputResp :: sMac :: sNextGroup :: sStoreOutputReq :: sNextPixel :: sDone :: Nil = Enum(10)
    
    val state = RegInit(sIdle)  
    val pixelIndex = row * N.U + col
    val packedInputIndex = pixelIndex * GP.U + groupIdx
    val inputAddr = inputBase + (packedInputIndex << 3)
    val kernelLoadAddr = kernelBase + (kernelLoadIdx << 3)  
    val outputAddr = outputBase + (pixelIndex << 3)  
    
    io.cmd.ready := !respValid
    
    io.resp.valid := respValid
    io.resp.bits.data := respData
    io.resp.bits.rd := respRd
    
    when (io.resp.fire()) {
      respValid := false.B
    }
    
    io.busy := false.B
    io.interrupt := false.B
    
    io.mem.req.valid := false.B
    io.mem.req.bits := DontCare
    
    when (io.cmd.fire()) {
      val rs1 = io.cmd.bits.rs1
      val cmdType = io.cmd.bits.rs2
      val rd = io.cmd.bits.inst.rd
      dprvReg := io.cmd.bits.status.dprv
      
      when (cmdType === 0.U) {
        inputBase := rs1
      } .elsewhen (cmdType === 1.U) {
        kernelBase := rs1
      } .elsewhen (cmdType === 2.U) {
        outputBase := rs1
      } .elsewhen (cmdType === 3.U) {
        when (!busyReg) {
          row := 0.U
          col := 0.U
          
          groupIdx := 0.U
          kernelLoadIdx := 0.U
          inputWord := 0.U

          acc := 0.U
          resultReg := 0.U
          for (i <- 0 until GP) {
            kernelBuf(i) := 0.U
          }
          
          doneReg := false.B
          busyReg := true.B
          errorReg := false.B
          state := sLoadKernelBufReq
        } .otherwise {
          errorReg := true.B
        }
      } .elsewhen (cmdType === 4.U) {
        respData := Cat(0.U((xLen - 3).W), busyReg, errorReg, doneReg)
        respRd := rd
        respValid := true.B
        
      } .elsewhen (cmdType === 4.U) {

        respData := resultReg
        respRd := rd
        respValid := true.B
      }
    }
    
    switch (state) {
      is (sIdle) {
      }
      
      is (sLoadKernelBufReq) {
        io.mem.req.valid := true.B
        io.mem.req.bits.addr := kernelLoadAddr
        io.mem.req.bits.cmd := M_XRD
        io.mem.req.bits.size := 3.U
        io.mem.req.bits.tag := 2.U
        
        io.mem.req.bits.signed := false.B
        io.mem.req.bits.data := 0.U
        io.mem.req.bits.phys := false.B
        io.mem.req.bits.dprv := dprvReg
                
        when (io.mem.req.fire()) {
          state := sLoadKernelBufResp
        }
      }
      
      is (sLoadKernelBufResp) {
        when (io.mem.resp.valid) {
          kernelBuf(kernelLoadIdx) := io.mem.resp.bits.data
          when (kernelLoadIdx === (GP - 1).U) {
            kernelLoadIdx := 0.U
            state := sLoadInputReq
          } .otherwise {
            kernelLoadIdx := kernelLoadIdx + 1.U
            state := sLoadKernelBufReq
          }
        }
      }
      
      is (sLoadInputReq) {
        io.mem.req.valid := true.B
        io.mem.req.bits.addr := inputAddr
        io.mem.req.bits.cmd := M_XRD
        io.mem.req.bits.size := 3.U
        io.mem.req.bits.tag := 1.U
        
        io.mem.req.bits.signed := false.B
        io.mem.req.bits.data := 0.U
        io.mem.req.bits.phys := false.B
        io.mem.req.bits.dprv := dprvReg
        
        when (io.mem.req.fire()) {
          state := sLoadInputResp
        }
      }
      
      is (sLoadInputResp) {
        when (io.mem.resp.valid) {
          inputWord := io.mem.resp.bits.data
          state := sMac
        }
      }
      
      is (sMac) {
        val kernelWord = kernelBuf(groupIdx)
        val a0 = inputWord(15, 0)
        val a1 = inputWord(31, 16)
        val a2 = inputWord(47, 32)
        val a3 = inputWord(63, 48)
      
        val b0 = kernelWord(15, 0)
        val b1 = kernelWord(31, 16)
        val b2 = kernelWord(47, 32)
        val b3 = kernelWord(63, 48)
        
        val p0 = (a0 * b0) >> 8
        val p1 = (a1 * b1) >> 8
        val p2 = (a2 * b2) >> 8
        val p3 = (a3 * b3) >> 8
        
        val partial = p0 +& p1 +& p2 +& p3
        val nextAcc = acc + partial.asUInt
        acc := nextAcc
        when (groupIdx === (GP - 1).U) {
          resultReg := nextAcc
          state := sStoreOutputReq
        } .otherwise {
          //groupIdx := groupIdx + 1.U
          state := sNextGroup
        }
        
      }
      
      is (sNextGroup) {
        groupIdx := groupIdx + 1.U
        state := sLoadInputReq

      }
      
      is (sStoreOutputReq) {
        io.mem.req.valid := true.B
        io.mem.req.bits.addr := outputAddr
        io.mem.req.bits.cmd := M_XWR
        io.mem.req.bits.size := 3.U
        io.mem.req.bits.tag := 3.U
        
        io.mem.req.bits.signed := false.B
        io.mem.req.bits.data := resultReg
        io.mem.req.bits.phys := false.B
        io.mem.req.bits.dprv := dprvReg  
        when (io.mem.req.fire()) {
          state := sNextPixel
        }    
      }
      
      is (sNextPixel) {
        acc := 0.U
        resultReg := 0.U
        groupIdx := 0.U
        
        when (col === (N - 1).U) {
          col := 0.U
          
          when (row === (N - 1).U) {
            row := 0.U
            state := sDone
          } .otherwise {
            row := row + 1.U
            state := sLoadInputReq
          }
        } .otherwise {
          col := col + 1.U
          state := sLoadInputReq
        }
      }
      
      is (sDone) {
        doneReg := true.B
        busyReg := false.B
        state := sIdle
      }             
    }
  }
  


    
    
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
     
