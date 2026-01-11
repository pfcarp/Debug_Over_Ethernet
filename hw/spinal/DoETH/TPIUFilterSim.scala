package doeth


import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{FlowMonitor, FlowDriver, ScoreboardInOrder}


object TPIUFilterSim extends App {

  Config.sim.compile({
    val dut = TPIUFilter(32)
    dut
  }).doSim { dut =>
    SimTimeout(10000)

    val scoreboard = ScoreboardInOrder[BigInt]()
    val rand = new scala.util.Random
    
    /*  */
    var bootstrapped = false
    FlowDriver(dut.io.push, dut.clockDomain) { payload =>
      if (!bootstrapped) {
        payload #= TPIUFilter.Sync.Full
        bootstrapped = true
      }
      else {
        if (rand.nextInt(100) < 50)
          payload #= TPIUFilter.Sync.Half
        else
          payload.randomize()
      }
      true
    }

    /*  */
    FlowMonitor(dut.io.push, dut.clockDomain) { payload =>
      if ((payload.toBigInt != TPIUFilter.Sync.Full) && (payload.toBigInt != TPIUFilter.Sync.Half)) {
        scoreboard.pushRef(payload.toBigInt)
      }
    }

    /*  */
    FlowMonitor(dut.io.pop, dut.clockDomain) { payload =>
      scoreboard.pushDut(payload.toBigInt)
      assert(
        assertion = (payload.toBigInt != TPIUFilter.Sync.Full) && (payload.toBigInt != TPIUFilter.Sync.Half),
        message   = "Received payload should be neither Half or Full sync!"
      )
    }

    dut.clockDomain.forkStimulus(10)
    
    dut.clockDomain.waitActiveEdgeWhere(scoreboard.matches == 100)
  }
}
