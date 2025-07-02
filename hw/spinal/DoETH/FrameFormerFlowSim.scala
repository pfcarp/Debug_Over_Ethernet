package doeth

import spinal.core._
import spinal.core.sim._

case class FrameFormerFlowSimModule(Input_Width: Int, Output_Width: Int, Max_Internal_Space: Int) extends FrameFormerFlow(Input_Width, Output_Width, Max_Internal_Space) {
  
  def sendRandomPayload () : Unit = {
        this.io.Subordinate.payload.randomize()
        this.io.Subordinate.valid #= true
        // this.io.Manager.ready #= false

        this.clockDomain.waitRisingEdge()

        this.io.Subordinate.valid #= false
    }

    def waitForIdleAgain () : Unit = {
        this.io.Manager.ready #= true
        this.clockDomain.waitRisingEdge()
        while (this.managerClockArea.SendingFSM.stateReg.toBigInt != this.managerClockArea.SendingFSM.Idle.stateId ){
            this.clockDomain.waitRisingEdge()
            //println(this.SendingFSM.stateReg.toBigInt)
        }
    }

    def waitXcyclesAfterLeaving (wait: Int) : Unit = {
      this.clockDomain.waitRisingEdge()
      while(this.managerClockArea.SendingFSM.stateNext.toBigInt != 1){
      if(this.managerClockArea.SendingFSM.stateNext.toBigInt != this.managerClockArea.SendingFSM.stateReg.toBigInt){
        this.io.Manager.ready #= false
        for(i <- 1 to wait){
          this.clockDomain.waitRisingEdge()
        }
      }
      else{
          this.io.Manager.ready #= true
          this.clockDomain.waitRisingEdge()
      }
      }
    }

    def waitXcyclesBetweenPayload(wait: Int) : Unit = {
      var flip = true

      this.io.Manager.ready #= true
      this.clockDomain.waitRisingEdge()
      while(this.managerClockArea.SendingFSM.stateNext.toBigInt != 4){
        this.clockDomain.waitRisingEdge()
      }
      while(this.managerClockArea.SendingFSM.stateNext.toBigInt == 4){
      if(flip){
        this.io.Manager.ready #= false
        for(i <- 1 to wait){
          this.clockDomain.waitRisingEdge()
        }
        
      }
      else{
          this.io.Manager.ready #= true
          this.clockDomain.waitRisingEdge()
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
       val dut = FrameFormerFlowSimModule(32, 64, 4)
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
        val ps = 4


        dut.inputs_debug.Destination #= dest
        dut.inputs_debug.Source #= source
        dut.inputs_debug.LinkType #= lt
        dut.inputs_debug.StartWord #= sw
        dut.inputs_debug.EndWord #= ew
        dut.inputs_debug.PacketSize #= ps
        
        dut.clockDomain.forkStimulus(period = 5)
        dut.ManagerDomain.forkStimulus(period = 10)

        dut.SubordinateDomain.forkStimulus(period = 5)

        //we want to create random payload but also vary the timings of transactions to see the following scenarios
        //1. a singlepayload entering
        //2. multiple entries entering when empty
        //3. multiple entries entering when full
        //4. variations of downstream being ready in between states
        //5. sending and recieving
        dut.io.Subordinate.payload.randomize()
        dut.io.Subordinate.valid #= true
        dut.io.Manager.ready #= false


        

        var FinishingPacket: Boolean = dut.managerClockArea.SendingFSM.stateReg.toString == "Footer"

        dut.clockDomain.waitRisingEdge()
        dut.io.Subordinate.valid #= false


        dut.io.Manager.ready #= true

        dut.clockDomain.waitRisingEdge()

        dut.io.Subordinate.valid #= false
        dut.clockDomain.waitRisingEdge(2)
        println(dut.managerClockArea.SendingFSM.stateReg.toBigInt)

        //dut.clockDomain.waitSamplingWhere(dut.SendingFSM.stateReg.toString == "Payload")
        
        // if(FinishingPacket){
        //     dut.io.Subordinate.payload.randomize()
        //     dut.io.Subordinate.valid #= true
        // }

        while (dut.managerClockArea.SendingFSM.stateReg.toBigInt != dut.managerClockArea.SendingFSM.Idle.stateId ){
            dut.clockDomain.waitRisingEdge()
            // println(dut.SendingFSM.stateReg.toBigInt)
        }

        if(dut.io.Manager.valid==false){
            dut.io.Manager.ready #= false
        }

        for (i <- 1 to 5){
            dut.sendRandomPayload()
        }
        println("Hello")

        dut.io.Manager.ready #= true
        dut.waitForIdleAgain()

        for (i <- 1 to 5){
            dut.sendRandomPayload()
        }

        dut.io.Manager.ready #= true
        dut.waitXcyclesAfterLeaving(2)
        dut.waitForIdleAgain()


        for (i <- 1 to 5){
            dut.sendRandomPayload()
        }

        dut.waitXcyclesBetweenPayload(4)
        dut.waitForIdleAgain()
        
    }

    // def SendRandomPayload(dut: FrameFormer) : Unit = {
    //     dut.io.Subordinate.payload.randomize()
    //     dut.io.Subordinate.valid #= true
    //     dut.io.Manager.ready #= false

    //     dut.clockDomain.waitRisingEdge()

    //     dut.io.Subordinate.valid #= false
 
    // }

}