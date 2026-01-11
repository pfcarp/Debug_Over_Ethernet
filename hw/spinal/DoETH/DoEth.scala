package doeth


import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


case class DoEth(ingressWidth: Int, outgressWidth: Int) extends Component {

  val io = new Bundle {
    val push =  slave(  Flow(Bits(ingressWidth  bits)))
    val pop  = master(Stream(Bits(outgressWidth bits)))
  }

  val filter = TPIUFilter(ingressWidth)
  val queue  = MemQueue(ingressWidth, ((4 KiB)/ingressWidth).toInt)
  val flush  = PingPongBuffer(outgressWidth, ((4 KiB)/outgressWidth).toInt, 1024)

  filter.io.push << io.push
  queue.io.push << filter.io.pop.toStream
  StreamWidthAdapter(queue.io.pop, flush.io.push)
  io.pop << flush.io.pop

}

