package doeth

import spinal.core._
import spinal.core.sim._

case class FrameFormerFlowSimModule(Input_Width: Int, Output_Width: Int, Max_Internal_Space: Int) extends FrameFormerFlow(Input_Width, Output_Width, Max_Internal_Space) {
  def sendRandomPayload () : Unit = {
        this.io.Subordinate.payload.randomize()
        this.io.Subordinate.valid #= true
        // this.io.Manager.ready #= false

        this.SubordinateDomain.waitRisingEdge()

        this.io.Subordinate.valid #= false
    }

    def sendWordSync () : Unit = {
        this.io.Subordinate.payload#=0x7fffffff
        this.io.Subordinate.valid #= true
        // this.io.Manager.ready #= false

        this.SubordinateDomain.waitRisingEdge()

        this.io.Subordinate.valid #= false
    }

    def sendHalfWordSync () : Unit = {
        this.io.Subordinate.payload#=0x7fff7fff
        this.io.Subordinate.valid #= true
        // this.io.Manager.ready #= false

        this.SubordinateDomain.waitRisingEdge()

        this.io.Subordinate.valid #= false
    }

    def sendXDuplicatePayload (cycles: Int) : Unit = {
        for(i <- 1 to cycles){
          this.io.Subordinate.payload #= 0xdead
          this.io.Subordinate.valid #= true
          // this.io.Manager.ready #= false

          this.SubordinateDomain.waitRisingEdge()
        }

        this.io.Subordinate.valid #= false
    }


    def waitXcyclesBetweenSendingPayload(wait: Int) : Unit = {
      var flip = true

      this.io.Manager.ready #= true
      
      while(this.managerClockArea.SendingFSM.stateNext.toBigInt == 4){
      if(flip){
        this.io.Subordinate.valid #= false
        for(i <- 1 to wait){
          this.sendHalfWordSync()
        }
        
      }
      else{
          this.io.Subordinate.payload #= 0xdead
          this.io.Subordinate.valid #= true
          this.SubordinateDomain.waitRisingEdge()
      }
      flip = !flip
      }
    }

    def waitXcyclesBetweenSendingRandomPayload(wait: Int, packets: Int) : Unit = {
      var flip = true

      this.io.Manager.ready #= true
      this.SubordinateDomain.waitRisingEdge()
      // while(this.managerClockArea.SendingFSM.stateNext.toBigInt != 4){
      //   this.clockDomain.waitRisingEdge()
      // }
      //while(this.managerClockArea.SendingFSM.stateNext.toBigInt == 4){
      for(i <- 1 to packets){
      if(flip){
        this.io.Subordinate.valid #= false
        for(i <- 1 to wait){
          this.sendHalfWordSync()
        }
        
      }
      else{
          this.io.Subordinate.payload.randomize()
          this.io.Subordinate.valid #= true
          this.SubordinateDomain.waitRisingEdge()
      }
      flip = !flip
      }
      //}
    }

    def waitForIdleAgain () : Unit = {
        this.io.Manager.ready #= true
        this.ManagerDomain.waitRisingEdge()
        while (this.managerClockArea.SendingFSM.stateReg.toBigInt != this.managerClockArea.SendingFSM.Idle.stateId ){
            this.ManagerDomain.waitRisingEdge()
            //println(this.SendingFSM.stateReg.toBigInt)
        }
    }

    def waitXcyclesAfterLeaving (wait: Int) : Unit = {
      this.ManagerDomain.waitRisingEdge()
      while(this.managerClockArea.SendingFSM.stateNext.toBigInt != 1){
      if(this.managerClockArea.SendingFSM.stateNext.toBigInt != this.managerClockArea.SendingFSM.stateReg.toBigInt){
        this.io.Manager.ready #= false
        for(i <- 1 to wait){
          this.ManagerDomain.waitRisingEdge()
        }
      }
      else{
          this.io.Manager.ready #= true
          this.ManagerDomain.waitRisingEdge()
      }
      }
    }

    def waitXcyclesBetweenPayload(wait: Int) : Unit = {
      var flip = true

      this.io.Manager.ready #= true
      this.ManagerDomain.waitRisingEdge()
      while(this.managerClockArea.SendingFSM.stateNext.toBigInt != 4){
        this.ManagerDomain.waitRisingEdge()
      }
      while(this.managerClockArea.SendingFSM.stateNext.toBigInt == 4){
      if(flip){
        this.io.Manager.ready #= false
        for(i <- 1 to wait){
          this.ManagerDomain.waitRisingEdge()
        }
        
      }
      else{
          this.io.Manager.ready #= true
          this.ManagerDomain.waitRisingEdge()
      }
      flip = !flip
      }
    }
}

// object FrameFormerSimModuleVerilogGen extends App {
//   val inputWidth = 64
//   val outputWidth = 64
//   val maxInternalSpace = 128

//   Config.spinal.generateVerilog(FrameFormerSimModule(
//       inputWidth,
//       outputWidth,
//       maxInternalSpace
//     )
//   )
// }


object FrameFormerFlowSim extends App {
    Config.sim.compile({
       val dut = FrameFormerFlowSimModule(32, 64, 16)
        dut.managerClockArea.SendingFSM.stateReg.simPublic()
        dut.managerClockArea.SendingFSM.stateNext.simPublic()
        dut
    }).doSim {dut =>


        //abcdef
        val dest = BigInt("DEADBEEFCAFE", 16)
        val source = BigInt("ABADFACEBEAD", 16)
        val lt = 0x1337
        val sw = 0xBAAB
        val ew = 0xBEEB
        val ps = 16


        dut.inputs_debug.Destination #= dest
        dut.inputs_debug.Source #= source
        dut.inputs_debug.LinkType #= lt
        dut.inputs_debug.StartWord #= sw
        dut.inputs_debug.EndWord #= ew
        dut.inputs_debug.PacketSize #= ps
        
        dut.clockDomain.forkStimulus(period = 6)
        dut.ManagerDomain.forkStimulus(period = 10)

        dut.SubordinateDomain.forkStimulus(period = 5)

  
        dut.waitForIdleAgain()
        dut.sendRandomPayload()
        dut.sendHalfWordSync()
        dut.sendWordSync()
        

        dut.sendRandomPayload()
        dut.sendRandomPayload()
        dut.sendRandomPayload()
        dut.sendRandomPayload()
        dut.sendRandomPayload()

        dut.sendHalfWordSync()
        dut.SubordinateDomain.waitRisingEdge(10)

        dut.sendWordSync()
        dut.sendRandomPayload()
        dut.sendWordSync()
        dut.sendRandomPayload()
        dut.sendWordSync()
        dut.sendRandomPayload()
        dut.sendRandomPayload()

        dut.waitForIdleAgain()

    }

}