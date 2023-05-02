package tb.amba4.AxiLite4

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.Stream
import spinal.lib.bus.amba4.axilite.AxiLite4R
import tb.Transaction
import tb.driver.StreamSource

import scala.util.Random.nextInt

case class AxiLite4RSource(bus: Stream[AxiLite4R], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends StreamSource(bus, clockDomain, queueOccupancyLimit) {
  var flowPercent: Int = 100

  override def sendData[T <: Transaction](transaction: T): Unit = {
    val pkg = transaction.asInstanceOf[AxiLite4RPkg]
    bus.resp #= pkg.resp
    bus.data #= pkg.data
  }

  override def streamPause(): Boolean = nextInt(100) > (flowPercent - 1)

  def setFlowPercent(flowPercent: Int) = {
    assert(flowPercent > 0 && flowPercent <= 100, s"flowPercent should be [1:100]")
    log.info(s"set flow percent :$flowPercent")
    this.flowPercent = flowPercent
  }
}
