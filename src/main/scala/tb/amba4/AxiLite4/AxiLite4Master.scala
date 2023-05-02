package tb.amba4.AxiLite4

import spinal.core._
import spinal.lib.bus.amba4.axilite._
import tb.Event
import tb.memory._

case class AxiLite4Master(bus: AxiLite4, clockDomain: ClockDomain, queueOccupancyLimit: Int) extends Region(BigInt(1) << bus.config.addressWidth, 0) {
  val writeDrv = AxiLite4WriteOnlyMaster(bus.aw, bus.w, bus.b, clockDomain, queueOccupancyLimit)
  val readDrv = AxiLite4ReadOnlyMaster(bus.ar, bus.r, clockDomain, queueOccupancyLimit)

  def init() = {
    writeDrv.init()
    readDrv.init()
  }

  def start() = {
    writeDrv.start()
    readDrv.start()
  }

  def stop() = {
    writeDrv.stop()
    readDrv.stop()
  }

  def idle = writeDrv.idle && readDrv.idle

  override def writePrivate(address: BigInt, data: Array[Byte], kwargs: Map[String, BigInt]): Unit = {
    writeDrv.writePrivate(address, data, kwargs)
  }

  override def readPrivate(address: BigInt, length: Int, kwargs: Map[String, BigInt]): Array[Byte] = {
    readDrv.readPrivate(address, length, kwargs)
  }

  def initRead(address: BigInt, length: Int, prot: BigInt): Event = {
    readDrv.initRead(address, length, prot)
  }

  def initWrite(address: BigInt, data: Array[Byte], prot: BigInt): Event = {
    writeDrv.initWrite(address, data, prot)
  }

  def setFlowPercent(awFlowPercent: Int, wflowPercent: Int, bflowPercent: Int, arFlowPercent: Int, rflowPercent: Int) = {
    writeDrv.setFlowPercent(awFlowPercent, wflowPercent, bflowPercent)
    readDrv.setFlowPercent(arFlowPercent, rflowPercent)
  }

}
