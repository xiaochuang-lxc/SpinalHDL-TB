package tb.amba4.Axi4

import spinal.core.ClockDomain
import spinal.lib.bus.amba4.axi.Axi4
import tb.memory.Region

case class Axi4Slave(axi4: Axi4, clockDomain: ClockDomain, maxPkgPending: Int = 8, target: Region) {
  val writeDrv = Axi4WriteOnlySlave(axi4.aw, axi4.w, axi4.b, clockDomain, maxPkgPending, target)
  val readDrv = Axi4ReadOnlySlave(axi4.ar, axi4.r, clockDomain, maxPkgPending, target)

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

  def setFlowPercent(awFlowPercent: Int, wflowPercent: Int, bflowPercent: Int, arFlowPercent: Int, rFowPercent: Int) = {
    writeDrv.setFlowPercent(awFlowPercent, wflowPercent, bflowPercent)
    readDrv.setFlowPercent(arFlowPercent, rFowPercent)
  }

  def setPkgBasePause() = {
    writeDrv.setPkgBasePause()
    readDrv.setPkgBasePause()
  }

  def setStreamBasePause() = {
    writeDrv.setStreamBasePause()
    readDrv.setStreamBasePause()
  }

}
