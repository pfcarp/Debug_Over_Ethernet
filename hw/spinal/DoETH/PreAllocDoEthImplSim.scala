package doeth


import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{StreamMonitor, StreamDriver, StreamReadyRandomizer, FlowMonitor, FlowDriver, ScoreboardInOrder}


object PreAllocDoEthImplSim extends App {

  Config.sim.compile({
    val dut = PreAllocDoEthImpl(32, 64)
    dut
  }).doSim { dut =>
    SimTimeout(1000000000)

    val scoreboard = ScoreboardInOrder[BigInt]()
    val rand = new scala.util.Random
    
    /*  */
    var bootstrapped = false
    FlowDriver(dut.io.subordinate.push, dut.subordinateDomain) { payload =>
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
    FlowMonitor(dut.io.subordinate.push, dut.subordinateDomain) { payload =>
      if ((payload.toBigInt != TPIUFilter.Sync.Full) && (payload.toBigInt != TPIUFilter.Sync.Half)) {
        scoreboard.pushRef(payload.toBigInt)
      }
    }

    /* Simulate randomly available target */
    StreamReadyRandomizer(dut.io.manager.pop, dut.managerDomain).setFactor(1.0f)

    /*  */
    // StreamMonitor(dut.io.manager.pop, dut.managerDomain) { payload =>
    //   if (payload.toBigInt != BigInt("FFFFFFFFFFFFFFFF", 16)) {
    //     val lower = payload.toBigInt & BigInt("00000000FFFFFFFF", 16)
    //     val upper = (payload.toBigInt & BigInt("FFFFFFFF00000000", 16)) >> 32
    //     scoreboard.pushDut(lower)
    //     scoreboard.pushDut(upper)
    //   }
    // }

    dut.managerDomain.forkStimulus(156 MHz)
    dut.subordinateDomain.forkStimulus(250 MHz)
    
    // dut.managerDomain.waitActiveEdgeWhere(scoreboard.matches == 1024)
    dut.managerDomain.waitActiveEdge(1000)
  }
}
