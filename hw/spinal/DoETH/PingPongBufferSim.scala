package doeth


import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{StreamMonitor, StreamDriver, StreamReadyRandomizer, ScoreboardInOrder}


object PingPongBufferSim extends App {

  Config.sim.compile({
    val dut = PingPongBuffer(32, 8)
    dut
  }).doSim { dut =>
    SimTimeout(10000)

    val scoreboard = ScoreboardInOrder[BigInt]()
    
    /* Pushes random data into the queue */
    StreamDriver(dut.io.push, dut.clockDomain) { payload =>
      payload.randomize()
      true
    }.setFactor(0.3f)

    /* Simulate randomly available target */
    StreamReadyRandomizer(dut.io.pop, dut.clockDomain).setFactor(1.0f)

    /*  */
    StreamMonitor(dut.io.push, dut.clockDomain) { payload =>
      scoreboard.pushRef(payload.toBigInt)
    }

    /*  */
    StreamMonitor(dut.io.pop, dut.clockDomain) { payload =>
      scoreboard.pushDut(payload.toBigInt)
    }

    dut.clockDomain.forkStimulus(10)

    dut.clockDomain.waitActiveEdgeWhere(scoreboard.matches == 100)
  }
}
