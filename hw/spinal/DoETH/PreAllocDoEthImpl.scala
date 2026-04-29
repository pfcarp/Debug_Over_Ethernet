package doeth


import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import VerilogBusAttributeAdder._
import spinal.lib.bus.amba4.axis._



case class PreAllocDoEthImpl(ingressWidth: Int, outgressWidth: Int) extends Component {

  val io = new Bundle {
    val subordinate = new Bundle {
      val clock =    in(Bool())
      val reset =    in(Bool())
      val push  = slave(Flow(Bits(ingressWidth  bits)))
    }
    val manager = new Bundle {
      val clock =     in(Bool())
      val reset =     in(Bool())
      val pop   = master(Axi4Stream(Axi4StreamConfig(dataWidth = outgressWidth/8, useLast=true)))
    }
  }
  val debug = new Bundle {
      val buffHead0 = out(UInt(8 bits))
      val buffHead1 = out(UInt(8 bits))
      val errorState = out(Bool())
    }

  val subordinateDomain = ClockDomain(
    clock = io.subordinate.clock,
    reset = io.subordinate.reset,
    config = ClockDomainConfig(
      resetKind = ASYNC,
      resetActiveLevel = LOW
    )
  )

  val managerDomain = ClockDomain(
    clock = io.manager.clock,
    reset = io.manager.reset,
    config = ClockDomainConfig(
      resetKind = ASYNC,
      resetActiveLevel = LOW
    )
  )
  
  val cdc = new StreamFifoCC(Bits(outgressWidth bits), 2, subordinateDomain, managerDomain)

  val scd = new ClockingArea(subordinateDomain) {
    val filter = TPIUFilter(ingressWidth)
    debug.errorState := filter.debug.errorState
    val queue  = MemQueue(ingressWidth, ((4 KiB)/ingressWidth).toInt)
    filter.io.push << io.subordinate.push
    queue.io.push << filter.io.pop.toStream
    StreamWidthAdapter(queue.io.pop, cdc.io.push)
  }
  val pcd = new ClockingArea(managerDomain) {
    val flush  = PreAllocPingPongBuffer(outgressWidth, ((4 KiB)/outgressWidth).toInt, 1024)
    flush.io.push << cdc.io.pop
    io.manager.pop << flush.io.pop

    debug.buffHead0 := flush.debug.buffHead0
    debug.buffHead1 := flush.debug.buffHead1
  }

}

object DoethVerilogGen extends App {
  val inputWidth = 32
  val outputWidth = 64

  Config.spinal.generateVerilog({
    val doeth = new PreAllocDoEthImpl(
      inputWidth,
      outputWidth,
    )
    VerilogBusAttributeAdder(doeth.io.manager.pop)
    doeth
})
}
