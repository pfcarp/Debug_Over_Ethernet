package doeth


import spinal.core._
import spinal.lib._


case class MemQueue(dataWidth: Int, depth: Int) extends Component {

  val io = new Bundle {
    val push =  slave(Stream(Bits(dataWidth bits)))
    val pop  = master(Stream(Bits(dataWidth bits)))
  }

  // Registers
  val head  = Counter(log2Up(depth) bits, io.push.fire)
  val tail  = Counter(log2Up(depth) bits, io.pop.fire)
  val count = CounterUpDown(depth+1, io.push.fire, io.pop.fire, false)
  
  // Memory buffer
  val buffer = Mem(Bits(dataWidth bits), depth)

  // Push
  io.push.ready := count =/= depth
  buffer.write(head, io.push.payload, io.push.fire)

  // Pop
  io.pop.valid   := count =/= 0
  io.pop.payload := buffer.readAsync(tail)

}
