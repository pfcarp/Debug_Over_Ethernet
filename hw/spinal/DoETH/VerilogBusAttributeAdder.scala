package spinal.lib

import spinal.core._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.tools._
import spinal.lib.bus.amba4.axis._

object VerilogBusAttributeAdder {

	val x_interface_info = "X_INTERFACE_INFO"

	case class ChannelInfo(val busInfo: BusInfo, val channelName: String, axuStream: Boolean = false) {

		def apply[T <: Data](pin: T): Unit = {
			println(s"Adding interface info for pin: ${pin.getPartialName()} with channel name: $channelName and bus info: $busInfo (axuStream: $axuStream)")
        	pin.addAttribute(x_interface_info, f"${busInfo.interfaceType} ${busInfo.busName} ${if (axuStream) "T" else channelName}${pin.getPartialName().toUpperCase()}")
		}
	}

	case class BusInfo(val interfaceType: String, val busName: String, axuStream: Boolean = false) {

		private def streamElements(target: Stream[_ <: Bundle]) = {
			target.payload.elements.map(_._2) ++ Array(target.valid, target.ready)
		}

		def apply[T <: Stream[_ <: Bundle]](channel: T): Unit = {
			val info = ChannelInfo(this, channel.getPartialName().toUpperCase(), axuStream = axuStream)
			streamElements(channel).foreach(info(_))
		}
	}

	case class BusAnnotator(val interfaceType: String, val protocolName: String, axuStream: Boolean = false) {

		println(s"Creating BusAnnotator for interfaceType: $interfaceType, protocolName: $protocolName, axuStream: $axuStream")

		def apply[T <: Bundle with IMasterSlave](bus: T, channels: Array[Stream[_ <: Bundle]]) = {
			bus.addAttribute(x_interface_info, f"XIL_INTERFACENAME ${bus.getName()}, PROTOCOL ${protocolName}, MODE ${if (bus.isMasterInterface) "Master" else "Slave"}")
			val info = BusInfo(interfaceType, bus.getName(), axuStream = axuStream)
			channels.foreach(info(_))
		}
	}

	def apply(bus: Axi4Stream.Axi4Stream): Unit = {
		val interfaceType = "xilinx.com:interface:axis:1.0"
		val protocol = "AXI4STREAM"
		BusAnnotator(interfaceType = interfaceType, protocolName = protocol, axuStream = true)(bus, Array(bus))
	}

	def apply(bus: AxiLite4): Unit = {
		val interfaceType = "xilinx.com:interface:aximm:1.0"
		val protocol = "AXI4LITE"
		BusAnnotator(interfaceType = interfaceType, protocolName = protocol)(bus, Array(bus.ar, bus.r, bus.aw, bus.w, bus.b))
	}

	def apply(bus: Axi4): Unit = {
		val interfaceType = "xilinx.com:interface:aximm:1.0"
		val protocol = "AXI4"
		BusAnnotator(interfaceType = interfaceType, protocolName = protocol)(bus, Array(bus.ar, bus.r, bus.aw, bus.w, bus.b))
	}

}