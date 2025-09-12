package doeth


import scala.math._

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axis._


object FrameFormer {

  def apply(traceWidth: Int, traceClockDomain: ClockDomain, streamConfig: Axi4StreamConfig, streamClockDomain: ClockDomain, depth: Int): FrameFormer = {
    new FrameFormer(traceWidth, traceClockDomain, streamConfig, streamClockDomain, depth)
  }

  /*
  def apply(traceWidth: Int, traceClockDomain: ClockDomain, streamWidth: Int, streamClockDomain: ClockDomain, depth: Int): FrameFormer = {
    new FrameFormer(traceWidth, traceClockDomain, Axi4StreamConfig(streamWidth*8), streamClockDomain, depth)
  }
  */

}


class FrameFormer(traceWidth: Int, traceClockDomain: ClockDomain, streamConfig: Axi4StreamConfig, streamClockDomain: ClockDomain, depth: Int) extends Component {
  
  // traceWidth should always be 32 or 64 bits
  assert(
    assertion = (Seq(32, 64).contains(traceWidth)),
    message   = f"Trace bus width should be either 32 or 64 bits wide."
  )

  val io = new Bundle {
    val manager = new Bundle {
      val axis  = master(Axi4Stream(streamConfig))
      val ready =   out(Bool())
      val full  =   out(Bool())
      val empty =   out(Bool())
      val tail  =   out(UInt(log2Up(depth) bits))
    }
    val subordinate = new Bundle {
      val flow  = slave(Flow(Bits(traceWidth bits)))
      val full  =   out(Bool())
      val empty =   out(Bool())
      val tail  =   out(UInt(log2Up(depth) bits))
    }
    val debug = new Bundle {
      val destination = in(Bits(48 bits))
      val source      = in(Bits(48 bits))
      val linkType    = in(Bits(16 bits))
      val startWord   = in(Bits(16 bits))
      val endWord     = in(Bits(16 bits))
      val packetSize  = in(UInt(16 bits))
    }
  }

  //Stream to queue, when first item enters, put the input data then que data then end word
  
  /*things to keep in mind: 
  *check if the Widths match, if not then do use the Fragment library?
  * 
  * check state machine library
  * 
  */

  traceClockDomain.setSynchronousWith(streamClockDomain)

  io.manager.axis.keep := 0
  io.manager.axis.user := 0

  val queue = StreamFifoCC(
    dataType  = Bits(streamConfig.dataWidth*8 bits),
    depth     = depth,
    pushClock = traceClockDomain,
    popClock  = streamClockDomain
  )

  val subordinateClockArea = new ClockingArea(traceClockDomain) {

    queue.io.push.valid   := io.subordinate.flow.valid
    queue.io.push.payload := io.subordinate.flow.toReg().resized //Subordinate should feed directly into the queue
  
    io.subordinate.full  := queue.io.pushOccupancy === depth
    io.subordinate.empty := queue.io.pushOccupancy === 0
    io.subordinate.tail  := queue.io.pushOccupancy.resized //this is the current occupancy of the queue

  }

  val managerClockArea = new ClockingArea(streamClockDomain) {

    io.manager.full  := queue.io.popOccupancy === depth
    io.manager.empty := queue.io.popOccupancy === 0
    io.manager.tail  := queue.io.popOccupancy.resized //this is the current occupancy of the queue
  
    val fsm = new StateMachine {

      // I can already see a potential bug because it is checking if fired but the delay between states make put duplicates 
      val counter = Counter(8 bits) init(0)

      io.manager.axis.valid        := False
      io.manager.axis.payload.data := B(0).resized
      io.manager.axis.payload.last := False

      val idle: State = new State with EntryPoint {
        whenIsActive {
          io.manager.axis.valid        := False
          io.manager.axis.payload.last := False
          when (!io.manager.empty) {
            goto(headerPart1) 
          }
        }
      }

      val headerPart1: State = new State {
        whenIsActive {
          io.manager.axis.valid        := True
          io.manager.axis.payload.data := Cat(io.debug.source(0, 16 bits), io.debug.destination)
          io.manager.axis.payload.last := False
          when (io.manager.axis.fire) {
            goto(headerPart2)
          }
        }
      }

      val headerPart2: State = new State {
        whenIsActive {
          io.manager.axis.valid        := True
          io.manager.axis.payload.data := Cat(io.debug.startWord, io.debug.linkType, io.debug.source(16, 32 bits))
          when (io.manager.axis.fire) {
            goto(payload)
          }
        }
      }

      //Denis paraphrasing: make a separate state for loading zeros to the payload with a toggle
      val payload: State = new State {
        //I feel like there is a smarter way of doing this
        whenIsActive {
          io.manager.axis.valid := True      
          when (counter.value === io.debug.packetSize) {
            goto(footer) 
          }
          .elsewhen ((!io.manager.empty) && io.manager.axis.fire) {
            io.manager.axis.payload.data := queue.io.pop.payload //pop from the queue and send to the manager
            counter.increment()
          }
          .elsewhen (io.manager.empty && io.manager.axis.fire) {
            io.manager.axis.payload.data := B(0).resized //make this the correct type
            counter.increment()
          }
        }
      }

      val footer: State = new State {
        whenIsActive {
          io.manager.axis.valid        := True
          io.manager.axis.payload.data := io.debug.endWord.resized
          io.manager.axis.payload.last := True
          when (io.manager.axis.fire) {
            counter.clear()
            when (io.manager.empty) {
              goto(idle)
            } 
            .otherwise {
              goto(headerPart1)
            }
          }
        }
      }

    }
  
    queue.io.pop.ready := (fsm.isActive(fsm.payload)) && io.manager.axis.isFree && (!io.manager.empty)
    io.manager.ready := fsm.isActive(fsm.payload) && io.manager.axis.isFree

  }
}
