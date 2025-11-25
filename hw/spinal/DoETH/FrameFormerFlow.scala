package doeth

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.fsm.State
import scala.math._

import VerilogBusAttributeAdder._
import spinal.lib.bus.amba4.axis._
import spinal.lib.bus.amba4.axi._
import spinal.core

//import spinal.core.sim._
//import spinal.core



// Hardware definition
class FrameFormerFlow(Input_Width: Int, Output_Width: Int, Max_Internal_Space: Int) extends Component {
  val io = new Bundle {
    //interfaces
    val Subordinate = slave Flow (Bits(Input_Width bits)) //is always 64or32 bits wide
    val Manager = master(Axi4Stream(Axi4StreamConfig(dataWidth = Output_Width/8, useLast=true)))//is always 64or32 bits wide
    // val clockSub = in Bool() //clock for the subordinate interface
    // val clockMan = in Bool() //clock for the manager interface
    // val resetSub = in Bool() //reset for the subordinate interface
    // val resetMan = in Bool() //reset for the manager interface
    val axi = slave (Axi4(Axi4Config(
      addressWidth              =    40,
      dataWidth                 =   128,
      idWidth                   =    16,
      useId                     =  true,
      useRegion                 = false,
      useBurst                  =  true,
      useLock                   =  true,
      useCache                  =  true,
      useSize                   =  true,
      useQos                    =  true,
      useLen                    =  true,
      useLast                   =  true,
      useResp                   =  true,
      useProt                   =  true,
      useStrb                   =  true,
      useAllStrb                = false,
      arUserWidth               =    16,
      awUserWidth               =    16,
      rUserWidth                =    -1,
      wUserWidth                =    -1,
      bUserWidth                =    -1,
      readIssuingCapability     =     8,
      writeIssuingCapability    =     8,
      combinedIssuingCapability =    16,
      readDataReorderingDepth   =    -1
    )))
  }
  val inputs_debug = new Bundle {
    //configurable input parameters
    val Destination = in (Bits(48 bits))
    val Source = in (Bits(48 bits))
    val LinkType = in (Bits(16 bits))
    val StartWord = in (Bits(16 bits))
    val EndWord = in (Bits(16 bits))
    val PacketSize = in (UInt(16 bits))
    
    //timers in respect to queue size
    // val PacketThreshold = in (UInt((log10(Max_Internal_Space.asInstanceOf[Double]) / log10(2.0)).toInt bits))
    // val TimerThreshold = in (UInt((log10(Max_Internal_Space.asInstanceOf[Double]) / log10(2.0)).toInt bits))
    // val AdditionalPacketGap = in (UInt((log10(Max_Internal_Space.asInstanceOf[Double]) / log10(2.0)).toInt bits))

    //debug
    val FFMisReady = out (Bool())
    val FFSisFull = out (Bool())
    val FFSisEmpty = out (Bool())
    val FFSQueueTail = out (UInt((log10(Max_Internal_Space.asInstanceOf[Double]) / log10(2.0)).toInt bits))
    val FFMisFull = out (Bool())
    val FFMisEmpty = out (Bool())
    val FFMQueueTail = out (UInt((log10(Max_Internal_Space.asInstanceOf[Double]) / log10(2.0)).toInt bits))

  }
  println(io.Manager.config.dataWidth+" "+ Output_Width)
  //Stream to queue, when first item enters, put the input data then que data then end word
  
  /*things to keep in mind: 
  *check if the Widths match, if not then do use the Fragment library?
  * 
  * check state machine library
  * 
  */
  // val EmptyStream = Stream (Bits(Input_Width bits))
  // EmptyStream.payload := 0
  // EmptyStream.valid := True


  // val ManagerDomain = ClockDomain(
  //   clock = io.clockMan,
  //   reset = io.resetMan,
  //   config = ClockDomainConfig(
  //     resetKind = ASYNC,
  //     resetActiveLevel = LOW
  //   )
  // )
  val ManagerDomain = ClockDomain.external(
    name = "Manager",
    config = ClockDomainConfig(
      resetKind = ASYNC,
      resetActiveLevel = LOW
    )
  )


  // val SubordinateDomain = ClockDomain(
  //   clock = io.clockSub,
  //   reset = io.resetSub,
  //   //clockEnable = ManagerDomain.readResetWire,
  //   config = ClockDomainConfig(
  //     resetKind = ASYNC,
  //     resetActiveLevel = LOW
  //   )
  // )

  val SubordinateDomain = ClockDomain.external(
    name = "Subordinate",
    config = ClockDomainConfig(
      resetKind = ASYNC,
      resetActiveLevel = LOW
    )
  )

  //SubordinateDomain.setSynchronousWith(ManagerDomain)

  val Previous = Bits(32 bits)

  val BufferQueue = new StreamFifoCC(
    dataType = Bits(Input_Width bits),
    depth = Max_Internal_Space,
    pushClock =  SubordinateDomain,
    popClock = ManagerDomain,
    withPopBufferedReset = true
  )

  val subordinateClockArea = new ClockingArea(SubordinateDomain) {
    
    val configPort = Axi4SlaveFactory(io.axi)

    val Overflow = Reg(UInt(32 bits)) init(0)

    configPort.readAndWrite(Overflow,address=BigInt("A0000000",16))

    when(!(io.Subordinate.payload === 0x7fffffff || io.Subordinate.payload === 0x7fff7fff) & inputs_debug.FFSisFull){
      Overflow:=Overflow+1
    }
  // BufferQueue.io.push << io.Subordinate.toStream.resized //Subordinate should feed directly into the queue

  //now recieving is a state machine based on the recieving of the full sync word then expected subsequent data burst 
  //if the packet is a halfword sync then ignore the data and stay within state
  //only consume data if there data beat is within 4 beats from the full sync
  //if there is a full sync in between 4 packets that can be ignored (e.g. fs,d1,fs,d2,d3,d4 == fs,d1,d2,d3,d4 but hs,d1,fs,d2,d3,fs,d4,d5 == fs,d2,d3,d4,d5)
 val RecievingFSM = new StateMachine{
  BufferQueue.io.push.payload := B("0").resized
  BufferQueue.io.push.valid := False
  val Idle: State = new State with EntryPoint{
    whenIsActive{
      when(io.Subordinate.payload === 0x7fffffff){
        goto(recieving1stBeat)
      }
    }
  }
  val recieving1stBeat: State = new State{
    whenIsActive{
      when(!(io.Subordinate.payload === 0x7fffffff || io.Subordinate.payload === 0x7fff7fff)){
        BufferQueue.io.push.payload := io.Subordinate.payload
        BufferQueue.io.push.valid := True
        goto(recieving2ndBeat)
      }otherwise{
        BufferQueue.io.push.valid := False
      }
    }
  }
  val recieving2ndBeat: State = new State{
    whenIsActive{
      when(!(io.Subordinate.payload === 0x7fffffff || io.Subordinate.payload === 0x7fff7fff)){
        BufferQueue.io.push.payload := io.Subordinate.payload
        BufferQueue.io.push.valid := True
        goto(recieving3rdBeat)
      }otherwise{
        BufferQueue.io.push.valid := False
      }
    }

  }
  val recieving3rdBeat: State = new State{
    whenIsActive{
      when(!(io.Subordinate.payload === 0x7fffffff || io.Subordinate.payload === 0x7fff7fff)){
        BufferQueue.io.push.payload := io.Subordinate.payload
        BufferQueue.io.push.valid := True
        goto(recieving4thBeat)
      }otherwise{
        BufferQueue.io.push.valid := False
      }
    }
  }
  val recieving4thBeat: State = new State{
    whenIsActive{
      when(!(io.Subordinate.payload === 0x7fffffff || io.Subordinate.payload === 0x7fff7fff)){
        BufferQueue.io.push.payload := io.Subordinate.payload
        BufferQueue.io.push.valid := True
        goto(Idle)
      }otherwise{
        BufferQueue.io.push.valid := False
      }
    }
  }
 }

  
  inputs_debug.FFSisFull := BufferQueue.io.pushOccupancy === Max_Internal_Space
  inputs_debug.FFSisEmpty := BufferQueue.io.pushOccupancy === 0

  inputs_debug.FFSQueueTail := BufferQueue.io.pushOccupancy.resized //this is the current occupancy of the queue

  }

val managerClockArea = new ClockingArea(ManagerDomain) {
  inputs_debug.FFMisFull := BufferQueue.io.popOccupancy === Max_Internal_Space
  inputs_debug.FFMisEmpty := BufferQueue.io.popOccupancy === 0

  inputs_debug.FFMQueueTail := BufferQueue.io.popOccupancy.resized //this is the current occupancy of the queue
  

  val SendingFSM = new StateMachine{// I can already see a potential bug because it is checking if fired but the delay between states make put duplicates 
    val counter = Reg(UInt(8 bits)) init(0)
    io.Manager.payload.data := B(1)#*Output_Width
    io.Manager.valid := False
    io.Manager.payload.last := False
    //BufferQueue.io.pop.ready := False       

    //val arbiteredStream = StreamArbiterFactory.lowerFirst.transactionLock.onArgs(BufferQueue.io.pop.haltWhen(inputs_debug.FFSisEmpty | counter === inputs_debug.PacketSize),EmptyStream)

    

    val Idle: State = new State with EntryPoint{
      whenIsActive{
        // io.Manager.payload := B(0).resized
        io.Manager.valid := False
        io.Manager.payload.last := False
        when(!inputs_debug.FFMisEmpty){
          goto(HeaderPart1) 
        }
      }
    }

    val HeaderPart1: State = new State{
      whenIsActive{
        io.Manager.payload.data := Cat(inputs_debug.Source(0, 16 bits),inputs_debug.Destination)
        io.Manager.valid := True
        io.Manager.payload.last := False
        when(io.Manager.fire){
   
          goto (HeaderPart2)
        }
      }
    }

    val HeaderPart2: State = new State{
      whenIsActive{
        io.Manager.payload.data := Cat(inputs_debug.StartWord,inputs_debug.LinkType,inputs_debug.Source(16, 32 bits))
        io.Manager.valid := True
        when(io.Manager.fire){
          goto(Payload)
        }
      }
    }


    //Denis paraphrasing: make a separate state for loading zeros to the payload with a toggle
    val Payload: State = new State{
      whenIsActive{//I feel like there is a smarter way of doing this
        //io.Manager << arbiteredStream
        //BufferQueue.io.pop.ready := False 
        io.Manager.valid := True      
        when(counter === inputs_debug.PacketSize){
          goto(Footer)
          
        }

        //just an otherwise statement 
        .elsewhen(!inputs_debug.FFMisEmpty & BufferQueue.io.pop.valid & io.Manager.fire){
          io.Manager.payload.data := BufferQueue.io.pop.payload.resized //pop from the queue and send to the manager
          //BufferQueue.io.pop.ready := True
          counter:= counter + 1
        }

        .elsewhen(inputs_debug.FFMisEmpty & io.Manager.fire){
          //BufferQueue.io.pop.ready := False
          io.Manager.payload.data := B(1)#*Output_Width//make this the correct type
          counter:= counter + 1
        } 

        // .otherwise{
        //   io.Manager.payload := Mux(inputs_debug.FFSisEmpty,B(0).resized,BufferQueue.io.pop.payload)
        // }
      }
    }

    val Footer: State = new State{
      whenIsActive{
        io.Manager.payload.data:= inputs_debug.EndWord.resized
        io.Manager.payload.last := True
        io.Manager.valid := True
        when(io.Manager.fire){
          //io.Manager.valid := False
          counter:=0
          when(inputs_debug.FFMisEmpty){
           goto(Idle)
          } 
          .otherwise {
            goto(HeaderPart1)
          }
        }
      }
    }
  }
  
  BufferQueue.io.pop.ready := (SendingFSM.isActive(SendingFSM.Payload)) & io.Manager.isFree & !inputs_debug.FFMisEmpty

  inputs_debug.FFMisReady := SendingFSM.isActive(SendingFSM.Payload) & io.Manager.isFree
}
  
}



object FrameFormerFlowVerilogGen extends App {
  val inputWidth = 32
  val outputWidth = 64
  val maxInternalSpace = 16

  Config.spinal.generateVerilog({
    val FF = new FrameFormerFlow(
      inputWidth,
      outputWidth,
      maxInternalSpace
    )
    VerilogBusAttributeAdder(FF.io.axi)
    VerilogBusAttributeAdder(FF.io.Manager) // Add bus attributes to the Manager interface
    FF
})
}

object FrameFormerFlowVHDLGen extends App {
  val inputWidth = 64
  val outputWidth = 64
  val maxInternalSpace = 128

  Config.spinal.generateVhdl({
    val FF = new FrameFormerFlow(
      inputWidth,
      outputWidth,
      maxInternalSpace
    )
    VerilogBusAttributeAdder(FF.io.Manager)
    FF
})
}