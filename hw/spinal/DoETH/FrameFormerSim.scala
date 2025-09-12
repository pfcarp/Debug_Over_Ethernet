package doeth


import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axis._


case class FrameFormerSimModule(traceWidth: Int, streamConfig: Axi4StreamConfig, depth: Int) extends Component {
  
  val io = new Bundle {
    val manager = new Bundle {
      val clock =     in(Bool())
      val reset =     in(Bool())
      val axis  = master(Axi4Stream(streamConfig))
      val ready =    out(Bool())
      val full  =    out(Bool())
      val empty =    out(Bool())
      val tail  =    out(UInt(log2Up(depth) bits))
    }
    val subordinate = new Bundle {
      val clock =    in(Bool())
      val reset =    in(Bool())
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

  val traceClockDomain = ClockDomain(clock = io.subordinate.clock, reset = io.subordinate.reset)
  val streamClockDomain = ClockDomain(clock = io.manager.clock    , reset = io.manager.reset    )

  val ff = FrameFormer(traceWidth, traceClockDomain, streamConfig, streamClockDomain, depth)
  
  io.manager.axis <> ff.io.manager.axis
  io.manager.ready <> ff.io.manager.ready
  io.manager.full <> ff.io.manager.full
  io.manager.empty <> ff.io.manager.empty
  io.manager.tail <> ff.io.manager.tail
  
  io.subordinate.flow <> ff.io.subordinate.flow
  io.subordinate.full <> ff.io.subordinate.full
  io.subordinate.empty <> ff.io.subordinate.empty
  io.subordinate.tail <> ff.io.subordinate.tail

  io.debug <> ff.io.debug


  def sendRandomPayload () : Unit = {
    this.io.subordinate.flow.valid #= true
    this.io.subordinate.flow.payload.randomize()
    this.io.manager.axis.ready     #= false
    traceClockDomain.waitRisingEdge()

    this.io.subordinate.flow.valid #= false
  }

  def waitForIdleAgain () : Unit = {
    this.io.manager.axis.ready #= true
    traceClockDomain.waitRisingEdge()
    
    traceClockDomain.waitRisingEdgeWhere(ff.managerClockArea.fsm.stateReg.toBigInt == ff.managerClockArea.fsm.idle.stateId)
  }

  def waitXcyclesAfterLeaving (wait: Int) : Unit = {
    traceClockDomain.waitRisingEdge()
  
    while(ff.managerClockArea.fsm.stateNext.toBigInt != 1) {
      if(ff.managerClockArea.fsm.stateNext.toBigInt != ff.managerClockArea.fsm.stateReg.toBigInt) {
        this.io.manager.axis.ready #= false
        traceClockDomain.waitRisingEdge(wait-1)
      }
      else{
        this.io.manager.axis.ready #= true
        traceClockDomain.waitRisingEdge()
      }
    }
  }

  def waitXcyclesBetweenPayload(wait: Int) : Unit = {
    var flip = true

    this.io.manager.axis.ready #= true
    traceClockDomain.waitRisingEdge()

    traceClockDomain.waitRisingEdgeWhere(ff.managerClockArea.fsm.stateNext.toBigInt == 4)
    
    while(ff.managerClockArea.fsm.stateNext.toBigInt == 4) {
      if(flip) {
        this.io.manager.axis.ready #= false
        traceClockDomain.waitRisingEdge(wait-1)
      }
      else{
          this.io.manager.axis.ready #= true
          traceClockDomain.waitRisingEdge()
      }
      flip = !flip
    }
  }

}


object FrameFormerSim extends App {
  Config.sim.compile({
    val dut = FrameFormerSimModule(32, Axi4StreamConfig(64), 4)
    dut.ff.managerClockArea.fsm.stateReg.simPublic()
    dut.ff.managerClockArea.fsm.stateNext.simPublic()
    dut
  }).doSim { dut =>

        dut.io.debug.destination #= BigInt("DEADBEEFCAFE", 16)
        dut.io.debug.source      #= BigInt("ABADFACEBEAD", 16)
        dut.io.debug.linkType    #= BigInt(        "1337", 16)
        dut.io.debug.startWord   #= BigInt(        "BAAB", 16)
        dut.io.debug.endWord     #= BigInt(        "BEEB", 16)
        dut.io.debug.packetSize  #= 4
        
        //we want to create random payload but also vary the timings of transactions to see the following scenarios
        //1. a singlepayload entering
        //2. multiple entries entering when empty
        //3. multiple entries entering when full
        //4. variations of downstream being ready in between states
        //5. sending and recieving
        dut.io.subordinate.flow.valid #= true
        dut.io.subordinate.flow.payload.randomize()
        dut.io.manager.axis.ready     #= false
        dut.clockDomain.waitRisingEdge()

        dut.io.subordinate.flow.valid #= false
        dut.io.manager.axis.ready     #= true
        dut.clockDomain.waitRisingEdge()

        dut.io.subordinate.flow.valid #= false
        dut.clockDomain.waitRisingEdge(2)

        dut.clockDomain.waitRisingEdgeWhere(dut.ff.managerClockArea.fsm.stateReg.toBigInt == dut.ff.managerClockArea.fsm.idle.stateId)

        if (!dut.io.manager.axis.valid.toBoolean) {
            dut.io.manager.axis.ready #= false
        }

        for (i <- 1 to 5) {
            dut.sendRandomPayload()
        }

        dut.io.manager.axis.ready #= true
        dut.waitForIdleAgain()

        for (i <- 1 to 5) {
            dut.sendRandomPayload()
        }

        dut.io.manager.axis.ready #= true
        dut.waitXcyclesAfterLeaving(2)
        dut.waitForIdleAgain()


        for (i <- 1 to 5) {
            dut.sendRandomPayload()
        }

        dut.waitXcyclesBetweenPayload(4)
        dut.waitForIdleAgain()
        
    }

}
