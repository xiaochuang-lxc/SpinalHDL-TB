package tb.amba4.AxiLite4

import spinal.core.ClockDomain
import spinal.lib.bus.amba4.axilite.AxiLite4
import tb.memory.Region

case class AxiLite4Slave(axiLite4: AxiLite4, clockDomain: ClockDomain, target: Region, queueOccupancyLimit: Int) {
  val axiLite4WriteOnlySlave = AxiLite4WriteOnlySlave(axiLite4.aw, axiLite4.w, axiLite4.b, target, clockDomain, queueOccupancyLimit)
  val axiLite4ReadOnlySlave = AxiLite4ReadOnlySlave(axiLite4.ar, axiLite4.r, clockDomain, target, queueOccupancyLimit)

  def init() = {
    axiLite4WriteOnlySlave.init()
    axiLite4ReadOnlySlave.init()
  }

  def start() = {
    axiLite4WriteOnlySlave.start()
    axiLite4ReadOnlySlave.start()
  }

  def idle = axiLite4ReadOnlySlave.idel && axiLite4WriteOnlySlave.idle

  def setFlowPercent(awFlowPercent: Int, wflowPercent: Int, bflowPercent: Int, arFlowPercent: Int, rflowPercent: Int) = {
    axiLite4WriteOnlySlave.setFlowPercent(awFlowPercent, wflowPercent, bflowPercent)
    axiLite4ReadOnlySlave.setFlowPercent(arFlowPercent, rflowPercent)
  }

}
