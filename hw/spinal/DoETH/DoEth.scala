package doeth


import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


case class DoEth(ingressWidth: Int, outgressWidth: Int) extends Component {

  val io = new Bundle {
    val subordinate = new Bundle {
      val clock =    in(Bool())
      val reset =    in(Bool())
      val push  = slave(Flow(Bits(ingressWidth  bits)))
    }
    val primary = new Bundle {
      val clock =     in(Bool())
      val reset =     in(Bool())
      val pop   = master(Stream(Bits(outgressWidth bits)))
    }
  }

  val subordinateDomain = ClockDomain(
    clock = io.subordinate.clock,
    reset = io.subordinate.reset,
    config = ClockDomainConfig(
      resetKind = ASYNC,
      resetActiveLevel = LOW
    )
  )

  val primaryDomain = ClockDomain(
    clock = io.primary.clock,
    reset = io.primary.reset,
    config = ClockDomainConfig(
      resetKind = ASYNC,
      resetActiveLevel = LOW
    )
  )
  
  val cdc = new StreamFifoCC(Bits(outgressWidth bits), 2, subordinateDomain, primaryDomain)

  val scd = new ClockingArea(subordinateDomain) {
    val filter = TPIUFilter(ingressWidth)
    val queue  = MemQueue(ingressWidth, ((4 KiB)/ingressWidth).toInt)
    filter.io.push << io.subordinate.push
    queue.io.push << filter.io.pop.toStream
    StreamWidthAdapter(queue.io.pop, cdc.io.push)
  }
  val pcd = new ClockingArea(primaryDomain) {
    val flush  = PingPongBuffer(outgressWidth, ((4 KiB)/outgressWidth).toInt, 1024)
    flush.io.push << cdc.io.pop
    io.primary.pop << flush.io.pop
  }

}

