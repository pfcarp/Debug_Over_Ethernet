package doeth


import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


case class FlushQueue(dataWidth: Int, depth: Int, timeout: Int) extends Component {

  val io = new Bundle {
    val enable =     in(Bool())
    val push   =  slave(Stream(Bits(dataWidth bits)))
    val pop    = master(Stream(Bits(dataWidth bits)))
  }

  // Timer
  val timer = Timeout(timeout)

  // Registers
  val head  = Counter(depth+1, io.push.fire)
  val tail  = Counter(depth  , io.pop.fire )
  
  // Memory buffer
  val buffer = Mem(Bits(dataWidth bits), depth)

  // Finite State Machine
  val fsm = new StateMachine {
    val insert: State = new State with EntryPoint {
      whenIsActive {
        when (timer || (io.push.fire && (head === depth-1))) {
          goto(flush)
        }
      }
    }
    val flush: State = new State {
      whenIsActive {
        when (io.pop.fire && (tail === depth-1)) {
          tail.clear()
          head.clear()
          goto(insert)
        }
      }
    }
  }

  // Timer control
  timer.clearWhen((!io.enable) || (io.enable && (head === 0) && fsm.isActive(fsm.insert)) || (io.enable && fsm.isActive(fsm.flush)))

  // Push
  io.push.ready  := fsm.isActive(fsm.insert)
  buffer.write(head.resized, io.push.payload, io.push.fire)

  // Pop
  io.pop.valid   := fsm.isActive(fsm.flush)
  io.pop.payload := Mux(tail >= head, B(dataWidth bits, default -> True), buffer.readAsync(tail.resized))

}
