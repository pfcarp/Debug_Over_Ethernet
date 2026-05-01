package doeth


import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import javax.print.attribute.standard.Destination

import spinal.lib.bus.amba4.axis._

//passthrough a bundle

case class PreAllocFlushQueue(dataWidth: Int, depth: Int, timeout: Int) extends Component {

  val io = new Bundle {
    val enable =     in(Bool())
    val push   =  slave(Stream(Bits(dataWidth bits)))
    val pop    = master(Axi4Stream(Axi4StreamConfig(dataWidth = dataWidth/8, useLast=true)))
  }

  val debug = new Bundle {
      val head = out(UInt(8 bits))
  }
  

  //define here then talk with denis to passthrough
  val part1 =BigInt("e717c8940282e717",16) //in (Bits(48 bits))
  val part2 = BigInt("baab1337c8940282",16)//in (Bits(48 bits))
  // val LinkType =BigInt("1337",16) //in (Bits(16 bits))
  // val StartWord =BigInt("baab",16) //in (Bits(16 bits))
  val part3 =BigInt("beeb",16)//in (Bits(16 bits))


  // val Destination ="c8940282e717".asHex
  // val Source = "c8940282e717".asHex
  // val LinkType ="1337".asHex
  // val StartWord ="baab".asHex
  // val EndWord ="beeb".asHex

  // val bitDestination = B(Destination, 48 bits)
  // val bitSource = B(Source, 48 bits)
  // val bitLinkType = B(LinkType, 16 bits)
  // val bitStartWord = B(StartWord, 16 bits)
  // val bitEndWord = B(EndWord, 16 bits)

  // Timer
  val timer = Timeout(timeout)

  // Registers
  val head  = Counter(2, depth-1, io.push.fire)
  val tail  = Counter(0, depth-1 , io.pop.fire )
  
  // Memory buffer
  val buffer = Mem(Bits(dataWidth bits), depth)

  buffer.initBigInt(
    Seq(part1,part2)
    ++ Seq.fill(depth-3)(BigInt(0)) 
    ++ Seq(part3)
  )


  debug.head := head.value.resized


  // Finite State Machine
  val fsm = new StateMachine {
    val insert: State = new State with EntryPoint {
      whenIsActive {
        when (timer || (io.push.fire && (head === depth-2))) {
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
  timer.clearWhen((!io.enable) || (io.enable && (head === 2) && fsm.isActive(fsm.insert)) || (io.enable && fsm.isActive(fsm.flush)))

  // Push
  io.push.ready  := fsm.isActive(fsm.insert)
  buffer.write(head.resized, io.push.payload, io.push.fire)

  // Pop
  io.pop.valid   := fsm.isActive(fsm.flush)
  io.pop.payload.data := Mux(tail >= head.value && tail=/=depth-1, B(dataWidth bits, default -> True), buffer.readAsync(tail.resized))//should the endword end the packet or the range of the useful data?
  io.pop.payload.last := Mux(io.pop.fire && (tail === depth-1),True,False)
}
