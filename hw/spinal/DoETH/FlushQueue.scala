package doeth


import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


case class FlushQueue(dataWidth: Int, depth: Int) extends Component {

  val io = new Bundle {
    val push =  slave(Stream(Bits(dataWidth bits)))
    val pop  = master(Stream(Bits(dataWidth bits)))
  }

  // State
  val fsm = new StateMachine {
    val insert: State = new State with EntryPoint {
      whenIsActive {
        when (io.push.fire && (head === depth-1)) {
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

  // Registers
  val head  = Counter(depth+1, io.push.fire)
  val tail  = Counter(depth  , io.pop.fire )
  
  // Memory buffer
  val buffer = Mem(Bits(dataWidth bits), depth)

  // Push
  io.push.ready  := fsm.isActive(fsm.insert)
  buffer.write(head.resized, io.push.payload, io.push.fire)

  // Pop
  io.pop.valid   := fsm.isActive(fsm.flush)
  io.pop.payload := buffer.readAsync(tail.resized)

}
