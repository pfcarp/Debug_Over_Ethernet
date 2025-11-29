package doeth

import spinal.core._
import spinal.core.sim._
import scala.io.Source

case class FrameFormerFlowSimModule(Input_Width: Int, Output_Width: Int, Max_Internal_Space: Int) extends FrameFormerFlow(Input_Width, Output_Width, Max_Internal_Space) {
  

  def sendPayloadFromFile (filename: String) : Unit = {
    val dataList = {
      val source = Source.fromFile(filename)
      try {
        source.getLines()
        .drop(2)  // Skip first two lines
        .map(_.split(","))  // Split each line by comma
        .filter(_.length >= 4)  // Ensure line has at least 4 columns
        .map(_(3))  // Get 4th column (0-indexed, so index 3)
        .toList
      } finally {
        source.close()
      }
    }
        this.io.Subordinate.valid #= true
        this.io.Manager.ready #= true
        for(data<-dataList){
          println(data)
          this.io.Subordinate.payload #= BigInt(data,16)
          
          // this.io.Manager.ready #= false
          // this.SubordinateDomain.waitRisingEdge()
          this.SubordinateDomain.waitRisingEdge()
        }
        
    }

  def sendRandomPayload () : Unit = {
        this.io.Subordinate.payload.randomize()
        this.io.Subordinate.valid #= true
        // this.io.Manager.ready #= false

        this.SubordinateDomain.waitRisingEdge()

        this.io.Subordinate.valid #= false
    }
    
    def sendWordAsync () : Unit = {
        this.io.Subordinate.payload#=0x7fffffff
        this.io.Subordinate.valid #= true
        // this.io.Manager.ready #= false

        this.SubordinateDomain.waitRisingEdge()

        this.io.Subordinate.valid #= false
    }

    def sendHalfWordAsync () : Unit = {
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
      
      this.SubordinateDomain.waitRisingEdge()
      while(this.managerClockArea.SendingFSM.stateNext.toBigInt != 4){
        this.SubordinateDomain.waitRisingEdge()
      }
      while(this.managerClockArea.SendingFSM.stateNext.toBigInt == 4){
      if(flip){
        this.io.Subordinate.valid #= false
        for(i <- 1 to wait){
          this.SubordinateDomain.waitRisingEdge()
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
          this.SubordinateDomain.waitRisingEdge()
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
        dut.ManagerDomain.waitRisingEdge()

        dut.sendPayloadFromFile("/home/Patrick/packetParsingDebug/ConfirmBeforeFF.csv")
        dut.waitForIdleAgain()

        //we want to create random payload but also vary the timings of transactions to see the following scenarios
        //1. a singlepayload entering
        //2. multiple entries entering when empty
        //3. multiple entries entering when full
        //4. variations of downstream being ready in between states
        //5. sending and recieving
        // dut.waitForIdleAgain()
        // dut.sendRandomPayload()
        // dut.sendHalfWordAsync()
        // dut.sendWordAsync()

        
        // //dut.ManagerDomain.waitRisingEdgeWhere(dut.managerClockArea.SendingFSM.stateReg.toString == "Payload")
        // //dut.ManagerDomain.waitActiveEdgeWhere(dut.managerClockArea.SendingFSM.stateReg.toString == "Payload")
        // dut.ManagerDomain.waitRisingEdge(10)

        // dut.sendHalfWordAsync()

        // dut.waitXcyclesBetweenSendingRandomPayload(3,15)
        // // while (dut.managerClockArea.SendingFSM.stateReg.toString != "HeaderPart2"){
        // //   dut.ManagerDomain.waitRisingEdge()
        // // }
        
        
        
        // dut.sendXDuplicatePayload(5)
        //   for (i <- 1 to 2){
        //       dut.sendRandomPayload()
        //   }
        //   // dut.waitForIdleAgain()

        //   dut.waitXcyclesBetweenSendingPayload(2)
        //   dut.waitForIdleAgain()

    }

}