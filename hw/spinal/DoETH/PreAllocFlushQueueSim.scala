package doeth


import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{StreamMonitor, StreamDriver, StreamReadyRandomizer, ScoreboardInOrder}


object PreAllocFlushQueueSim extends App {

  Config.sim.compile({
    val dut = PreAllocFlushQueue(64, 8, 20)
    dut.tail.value.simPublic()
    dut
  }).doSim { dut =>
    SimTimeout(1000000)

    val scoreboard = ScoreboardInOrder[BigInt]()

    dut.io.enable #= true

    /* Pushes random data into the queue */
    StreamDriver(dut.io.push, dut.clockDomain) { payload =>
      payload.randomize()
      true
    }.setFactor(0.4f)

    /* Simulate randomly available target */
    StreamReadyRandomizer(dut.io.pop, dut.clockDomain).setFactor(1.0f)

    /*  */
    StreamMonitor(dut.io.push, dut.clockDomain) { payload =>
      scoreboard.pushRef(payload.toBigInt)
    }

    // /*  */
    // StreamMonitor(dut.io.pop, dut.clockDomain) { payload =>
    //   if (((dut.tail>6)||(dut.tail<1)))scoreboard.pushDut(payload.toBigInt)
    // }

    dut.clockDomain.forkStimulus(10)

    dut.clockDomain.waitActiveEdgeWhere(scoreboard.matches == 100)
    // dut.clockDomain.waitActiveEdge(100)
  }
}
