package tb.amba4.AxiLite4

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.Stream
import spinal.lib.bus.amba4.axilite.AxiLite4B
import tb.Transaction
import tb.driver.StreamSource

import scala.util.Random.nextInt

case class AxiLite4BSource(bus: Stream[AxiLite4B], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends StreamSource(bus, clockDomain, queueOccupancyLimit) {
  var flowPercent: Int = 100

  override def sendData[T <: Transaction](transaction: T): Unit = {
    val pkg = transaction.asInstanceOf[AxiLite4BPkg]
    bus.resp #= pkg.resp
  }

  override def streamPause(): Boolean = nextInt(100) > (flowPercent - 1)

  def setFlowPercent(flowPercent: Int) = {
    assert(flowPercent > 0 && flowPercent <= 100, s"flowPercent should be [1:100]")
    log.info(s"set flow percent :$flowPercent")
    this.flowPercent = flowPercent
  }
}
