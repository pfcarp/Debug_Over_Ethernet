package doeth


import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.amba4.axilite.AxiLite4Utils.Axi4Rich


import ultrascaleplus.clock.PLClockingArea
import ultrascaleplus.ip.{Ethernet}


import zcu102._
import zcu102.io.ethernet._


case class EthernetBB() extends ZCU102(
  config    = new ZCU102Config(
    withPL_CLK0   = 250.00 MHz,
    withLPD_HPM0  =       true,
    with_GT0      =       true,
    withSI570_MGT = 156.25 MHz,
    withTRACE     =       true
  )
) {

  io.lpd.hpm0.associate(io.pl.clk0)

  val stream = Axi4Stream(Ethernet.AxiTXConfig)
  
  val ethernet = new PLClockingArea(io.pl.clk0) {
    val ctrl = Ethernet(Ethernet0)
    ctrl.io.axi <> Axi4Rich(io.lpd.hpm0).toLite(ctrl.io.axi.config)
    ctrl.io.refclk <> io.user.si570.mgt
    ctrl.io.gt <> io.gt0
    ctrl.io.tx.axis << stream
  }

  val frameformer = new FrameFormerFlow(32, stream.config.dataWidth, 1)
  frameformer.io.clockSub := // TODO: this clock domain clock
  frameformer.io.resetSub := // TODO; this clock domain reset
  frameformer.io.clockMan := ethernet.ctrl.io.tx.clk.o
  frameformer.io.resetMan := ethernet.ctrl.io.user.tx.reset
  frameformer.io.subordinate << Flow(io.trace) // If yes, who drives `valid`?
  // OR: frameformer.io.subordinate.valid := io.trace.ctl
  //     frameformer.io.subordinate.payload := io.trace.data
  stream << frameformer.io.manager

  this.generate()
}


object EthernetBBVerilog extends App {
  val report = Config.spinal.generateVerilog(EthernetBB())
  report.mergeRTLSource("mergedRTL")
}

object EthernetBBVhdl extends App {
  Config.spinal.generateVhdl(EthernetBB())
}
