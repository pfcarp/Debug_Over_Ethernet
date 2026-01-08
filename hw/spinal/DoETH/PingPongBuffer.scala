package doeth


import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


case class PingPongBuffer(dataWidth: Int, depth: Int) extends Component {

  val io = new Bundle {
    val push =  slave(Stream(Bits(dataWidth bits)))
    val pop  = master(Stream(Bits(dataWidth bits)))
  }

  val insert  = UInt(1 bits)
  val extract = UInt(1 bits)
  val buffers = Seq.fill(2)(MemQueue(dataWidth, depth))

  val insertionFSM = new StateMachine {
    val waitForPing: State = new State with EntryPoint {
      whenIsActive {
        when (buffers(0).io.push.ready) {
          goto(ping)
        }
      }
    }
    val ping: State = new State {
      whenIsActive {
        when (!buffers(0).io.push.ready && buffers(1).io.push.ready) {
          goto(pong)
        }
        .elsewhen(!buffers(0).io.push.ready) {
          goto(waitForPong)
        }
      }
    }
    val waitForPong: State = new State {
      whenIsActive {
        when (buffers(1).io.push.ready) {
          goto(pong)
        }
      }
    }
    val pong: State = new State {
      whenIsActive {
        when (!buffers(1).io.push.ready && buffers(0).io.push.ready) {
          goto(ping)
        }
        .elsewhen(!buffers(1).io.push.ready) {
          goto(waitForPing)
        }
      }
    }
  }

  val extractionFSM = new StateMachine {
    val waitForPing: State = new State with EntryPoint {
      whenIsActive {
        when (!buffers(0).io.push.ready) {
          goto(ping)
        }
      }
    }
    val ping: State = new State {
      whenIsActive {
        when (!buffers(0).io.pop.valid && !buffers(1).io.push.ready) {
          goto(pong)
        }
        .elsewhen(!buffers(0).io.pop.valid) {
          goto(waitForPong)
        }
      }
    }
    val waitForPong: State = new State {
      whenIsActive {
        when (!buffers(1).io.push.ready) {
          goto(pong)
        }
      }
    }
    val pong: State = new State {
      whenIsActive {
        when (!buffers(1).io.pop.valid && !buffers(0).io.push.ready) {
          goto(ping)
        }
        .elsewhen(!buffers(1).io.pop.valid) {
          goto(waitForPing)
        }
      }
    }
  }

  val mux     = StreamMux(extract, Vec.tabulate(2)(i => buffers(i).io.pop))
  val demux   = StreamDemux(io.push.haltWhen(insertionFSM.isActive(insertionFSM.waitForPing) || insertionFSM.isActive(insertionFSM.waitForPong)), insert, 2)

  insert := Mux(insertionFSM.isActive(insertionFSM.ping), U(0), U(1))
  buffers(0).io.push << demux(0)
  buffers(1).io.push << demux(1)
  extract := Mux(extractionFSM.isActive(extractionFSM.ping), U(0), U(1))
  io.pop << mux.haltWhen(extractionFSM.isActive(extractionFSM.waitForPing) || extractionFSM.isActive(extractionFSM.waitForPong))

}
