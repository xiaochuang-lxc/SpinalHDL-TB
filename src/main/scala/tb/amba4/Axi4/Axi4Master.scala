package tb.amba4.Axi4

import spinal.core.ClockDomain
import spinal.lib.bus.amba4.axi.Axi4
import tb.Event
import tb.memory.Region

case class Axi4Master(axi4: Axi4, clockDomain: ClockDomain, maxPkgPending: Int = 8) extends Region(BigInt(1) << axi4.config.addressWidth, 0) {
  val axi4WriteDrv = Axi4WriteOnlyMaster(axi4.aw, axi4.w, axi4.b, clockDomain, maxPkgPending)
  val axi4ReadDrv = Axi4ReadOnlyMaster(axi4.ar, axi4.r, clockDomain, maxPkgPending)

  def init() = {
    axi4WriteDrv.init()
    axi4ReadDrv.init()
  }

  def start(): Unit = {
    axi4WriteDrv.start()
    axi4ReadDrv.start()
  }

  def idle = axi4WriteDrv.idle && axi4ReadDrv.idle

  def setFlowPercent(awFlowPercent: Int, wflowPercent: Int, bflowPercent: Int, arFlowPercent: Int, rFowPercent: Int) = {
    axi4WriteDrv.setFlowPercent(awFlowPercent, wflowPercent, bflowPercent)
    axi4ReadDrv.setFlowPercent(arFlowPercent, rFowPercent)
  }

  def setPkgBasePause() = {
    axi4WriteDrv.setPkgBasePause()
    axi4ReadDrv.setPkgBasePause()
  }

  def setStreamBasePause() = {
    axi4WriteDrv.setStreamBasePause()
    axi4ReadDrv.setStreamBasePause()
  }

  override def writePrivate(address: BigInt, data: Array[Byte], kwargs: Map[String, BigInt]): Unit = {
    axi4WriteDrv.writePrivate(address, data, kwargs)
  }

  override def readPrivate(address: BigInt, length: Int, kwargs: Map[String, BigInt]): Array[Byte] = {
    axi4ReadDrv.readPrivate(address, length, kwargs)
  }

  def initRead(address: BigInt, length: Int, kwargs: Map[String, BigInt]): Event = {
    axi4ReadDrv.initRead(address, length, kwargs)
  }

  def initWrite(address: BigInt, data: Array[Byte], kwargs: Map[String, BigInt]): Event = {
    axi4WriteDrv.initWrite(address, data, kwargs)
  }

}
