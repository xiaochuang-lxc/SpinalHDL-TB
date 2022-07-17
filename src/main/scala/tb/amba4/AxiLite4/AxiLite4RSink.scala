package tb.amba4.AxiLite4

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import tb.Transaction
import tb.monitor.StreamSink

import scala.util.Random.nextInt

case class AxiLite4RSink(bus: Stream[AxiLite4R], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends StreamSink(bus, clockDomain, queueOccupancyLimit) {

  var flowPercent: Int = 100

  override def sample(): Transaction = {
    AxiLite4RPkg(bus.data.toBigInt, bus.resp.toInt)
  }

  override def streamPause(): Boolean = nextInt(100) >= (flowPercent - 1)

  def setFlowPercent(flowPercent: Int) = {
    assert(flowPercent > 0 && flowPercent <= 100, s"flowPercent should be [1:100]")
    log.info(s"set flow percent :$flowPercent")
    this.flowPercent = flowPercent
  }

}
