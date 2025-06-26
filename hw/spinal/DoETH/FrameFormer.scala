package doeth

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.fsm.State
import scala.math._

//import spinal.core.sim._
//import spinal.core



// Hardware definition
class FrameFormer(Input_Width: Int, Output_Width: Int, Max_Internal_Space: Int) extends Component {
  val io = new Bundle {
    //interfaces
    val Subordinate = slave Stream (Bits(Input_Width bits)) //is always 64or32 bits wide
    val Manager = master Stream (Bits(Output_Width bits))//is always 64or32 bits wide
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
    val QueueTail = out (UInt((log10(Max_Internal_Space.asInstanceOf[Double]) / log10(2.0)).toInt bits))

  }
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


  val BufferQueue = StreamFifo(
    dataType = Bits(Output_Width bits),
    latency = 1,
    depth = Max_Internal_Space
    )

  // if(Input_Width != Output_Width){
  //   BufferQueue.io.push << io.Subordinate //Subordinate should feed directly into the queue    }else{
  // }else {
  //     BufferQueue.io.push << io.Subordinate //Subordinate should feed directly into the queue    }
  // }
   BufferQueue.io.push << io.Subordinate //Subordinate should feed directly into the queue

  
  inputs_debug.FFSisFull := BufferQueue.io.occupancy === Max_Internal_Space
  inputs_debug.FFSisEmpty := BufferQueue.io.occupancy === 0

  inputs_debug.QueueTail := BufferQueue.io.occupancy.resized //this is the current occupancy of the queue
  

  val SendingFSM = new StateMachine{// I can already see a potential bug because it is checking if fired but the delay between states make put duplicates 
    val counter = Reg(UInt(8 bits)) init(0)
    io.Manager.payload := B(0).resized
    io.Manager.valid := False
    //BufferQueue.io.pop.ready := False       

    //val arbiteredStream = StreamArbiterFactory.lowerFirst.transactionLock.onArgs(BufferQueue.io.pop.haltWhen(inputs_debug.FFSisEmpty | counter === inputs_debug.PacketSize),EmptyStream)

    

    val Idle: State = new State with EntryPoint{
      whenIsActive{
        // io.Manager.payload := B(0).resized
        io.Manager.valid := False
        when(!inputs_debug.FFSisEmpty){
          goto(HeaderPart1) 
        }
      }
    }

    val HeaderPart1: State = new State{
      whenIsActive{
        io.Manager.payload := Cat(inputs_debug.Source(0, 16 bits),inputs_debug.Destination)
        io.Manager.valid := True
        when(io.Manager.fire){
          goto (HeaderPart2)
        }
      }
    }

    val HeaderPart2: State = new State{
      whenIsActive{
        io.Manager.payload := Cat(inputs_debug.StartWord,inputs_debug.LinkType,inputs_debug.Source(16, 32 bits))
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
        .elsewhen(!inputs_debug.FFSisEmpty & io.Manager.fire){
          io.Manager.payload := BufferQueue.io.pop.payload //pop from the queue and send to the manager
          //BufferQueue.io.pop.ready := True
          counter:= counter + 1
        }

        .elsewhen(inputs_debug.FFSisEmpty & io.Manager.fire){
          //BufferQueue.io.pop.ready := False
          io.Manager.payload := B(0).resized//make this the correct type
          counter:= counter + 1
        } 

        // .otherwise{
        //   io.Manager.payload := Mux(inputs_debug.FFSisEmpty,B(0).resized,BufferQueue.io.pop.payload)
        // }
      }
    }

    val Footer: State = new State{
      whenIsActive{
        io.Manager.payload:= inputs_debug.EndWord.resized
        io.Manager.valid := True
        when(io.Manager.fire){
          //io.Manager.valid := False
          counter:=0
          when(inputs_debug.FFSisEmpty){
           goto(Idle)
          } 
          .otherwise {
            goto(HeaderPart1)
          }
        }
      }
    }
  }
  
  BufferQueue.io.pop.ready := (SendingFSM.isActive(SendingFSM.Payload)) & io.Manager.isFree & !inputs_debug.FFSisEmpty

  inputs_debug.FFMisReady := SendingFSM.isActive(SendingFSM.Payload) & io.Manager.isFree

  
}



object FrameFormerVerilogGen extends App {
  val inputWidth = 64
  val outputWidth = 64
  val maxInternalSpace = 128

  Config.spinal.generateVerilog(new FrameFormer(
      inputWidth,
      outputWidth,
      maxInternalSpace
    )
  )
}

object FrameFormerVHDLGen extends App {
  val inputWidth = 64
  val outputWidth = 64
  val maxInternalSpace = 128

  Config.spinal.generateVhdl(new FrameFormer(
      inputWidth,
      outputWidth,
      maxInternalSpace
    )
  )
}