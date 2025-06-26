package doeth

import spinal.core._
import spinal.core.sim._


object FrameFormerSim extends App {
    Config.sim.compile({
       val dut = FrameFormer(64, 64, 4)
        dut.SendingFSM.stateReg.simPublic()
        dut.SendingFSM.stateNext.simPublic()
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
        

        dut.clockDomain.forkStimulus(period = 10)

        
        

        //we want to create random payload but also vary the timings of transactions to see the following scenarios
        //1. a singlepayload entering
        //2. multiple entries entering when empty
        //3. multiple entries entering when full
        //4. variations of downstream being ready in between states
        //5. sending and recieving
        dut.io.Subordinate.payload.randomize()
        dut.io.Subordinate.valid #= true
        dut.io.Manager.ready #= false


        

        var FinishingPacket: Boolean = dut.SendingFSM.stateReg.toString == "Footer"

        dut.clockDomain.waitRisingEdge()
        dut.io.Subordinate.valid #= false


        dut.io.Manager.ready #= true

        dut.clockDomain.waitRisingEdge()

        dut.io.Subordinate.valid #= false
        dut.clockDomain.waitRisingEdge(2)

        //dut.clockDomain.waitSamplingWhere(dut.SendingFSM.stateReg.toString == "Payload")
        
        // if(FinishingPacket){
        //     dut.io.Subordinate.payload.randomize()
        //     dut.io.Subordinate.valid #= true
        // }
        
        while (dut.SendingFSM.stateReg.toBigInt != dut.SendingFSM.Idle.stateId ){
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