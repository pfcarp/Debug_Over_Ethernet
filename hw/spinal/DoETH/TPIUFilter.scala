package doeth


import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


object TPIUFilter {

  object Sync {
    val Full: BigInt = BigInt("FFFFFFFF", 16)
    val Half: BigInt = BigInt("0000FFFF", 16)
  }

}


case class TPIUFilter(dataWidth: Int) extends Component {

  val io = new Bundle {
    val push =  slave(Flow(Bits(dataWidth bits)))
    val pop  = master(Flow(Bits(dataWidth bits)))
  }

  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        when (io.push.valid && (io.push.payload === B(TPIUFilter.Sync.Full))) {
          goto(waitingForBeat0)
        }
      }
    }
    val waitingForBeat0: State = new State {
      whenIsActive {
        when (io.push.valid && (io.push.payload === B(TPIUFilter.Sync.Full))) {
          goto(error)
        }
        .elsewhen (io.push.valid && (io.push.payload =/= B(TPIUFilter.Sync.Half))) {
          goto(waitingForBeat1)
        }
      }
    }
    val waitingForBeat1: State = new State {
      whenIsActive {
        when (io.push.valid && (io.push.payload === B(TPIUFilter.Sync.Full))) {
          goto(error)
        }
        .elsewhen (io.push.valid && (io.push.payload =/= B(TPIUFilter.Sync.Half))) {
          goto(waitingForBeat2)
        }
      }
    }
    val waitingForBeat2: State = new State {
      whenIsActive {
        when (io.push.valid && (io.push.payload === B(TPIUFilter.Sync.Full))) {
          goto(error)
        }
        .elsewhen (io.push.valid && (io.push.payload =/= B(TPIUFilter.Sync.Half))) {
          goto(waitingForBeat3)
        }
      }
    }
    val waitingForBeat3: State = new State {
      whenIsActive {
        when (io.push.valid && (io.push.payload === B(TPIUFilter.Sync.Full))) {
          goto(error)
        }
        .elsewhen (io.push.valid && (io.push.payload =/= B(TPIUFilter.Sync.Half))) {
          goto(waitingForBeat0)
        }
      }
    }
    val error: State = new State {}
  }

  io.pop << io.push.throwWhen(!(
    (fsm.isActive(fsm.waitingForBeat0) && fsm.isEntering(fsm.waitingForBeat1)) ||
    (fsm.isActive(fsm.waitingForBeat1) && fsm.isEntering(fsm.waitingForBeat2)) ||
    (fsm.isActive(fsm.waitingForBeat2) && fsm.isEntering(fsm.waitingForBeat3)) ||
    (fsm.isActive(fsm.waitingForBeat3) && fsm.isEntering(fsm.waitingForBeat0))
  ))
}
