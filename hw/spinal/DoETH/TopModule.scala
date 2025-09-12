package doeth


import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.amba4.axilite.AxiLite4Utils.Axi4Rich
import spinal.lib.bus.amba4.axis._


import ultrascaleplus.clock.PLClockingArea
import ultrascaleplus.ip.{Ethernet}


import zcu102._
import zcu102.io.ethernet._


case class TopModule() extends ZCU102(
  config    = new ZCU102Config(
    withPL_CLK0   = 250.00 MHz,
    withPL_CLK1   =  75.00 MHz,
    withLPD_HPM0  =       true,
    with_GT0      =       true,
    withSI570_MGT = 156.25 MHz,
    withTRACE     =       true
  )
) {

  io.lpd.hpm0.associate(io.pl.clk0)
  io.trace.clk.associate(io.pl.clk0)

  val stream = Axi4Stream(Ethernet.AxiTXConfig)
  
  val ethernet = new PLClockingArea(io.pl.clk0) {
    val ctrl = Ethernet(Ethernet0)
    ctrl.io.axi <> Axi4Rich(io.lpd.hpm0).toLite(ctrl.io.axi.config)
    ctrl.io.refclk <> io.user.si570.mgt
    ctrl.io.gt <> io.gt0
    ctrl.io.tx.axis << stream
  }

  val frameformer = new FrameFormer(
    traceWidth        = io.trace.bus.width,
    traceClockDomain  = io.trace.clk.domain,
    streamConfig      = stream.config,
    streamClockDomain = ethernet.ctrl.getClockDomain(),
    depth             = 4
  )
  // Configuration
  frameformer.io.debug.destination := B(BigInt("DEADBEEFCAFE", 16), 48 bits)
  frameformer.io.debug.source      := B(BigInt("ABADFACEBEAD", 16), 48 bits)
  frameformer.io.debug.linkType    := B(BigInt(        "1337", 16), 16 bits)
  frameformer.io.debug.startWord   := B(BigInt(        "BAAB", 16), 16 bits)
  frameformer.io.debug.endWord     := B(BigInt(        "BEEB", 16), 16 bits)
  frameformer.io.debug.packetSize  := U(                         4, 16 bits)
  // Connection
  frameformer.io.subordinate.flow  << io.trace.bus.asFlow
  frameformer.io.manager.axis  >> stream

  this.generate()
}


object TopModuleVerilog extends App {
  val report = Config.spinal.generateVerilog(TopModule())
  report.mergeRTLSource("mergedRTL")
}

object TopModuleVhdl extends App {
  Config.spinal.generateVhdl(TopModule())
}
