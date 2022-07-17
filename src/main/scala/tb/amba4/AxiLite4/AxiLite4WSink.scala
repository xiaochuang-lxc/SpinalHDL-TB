package tb.amba4.AxiLite4

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.Stream
import spinal.lib.bus.amba4.axilite.AxiLite4W
import tb.Transaction
import tb.monitor.StreamSink

import scala.util.Random.nextInt

case class AxiLite4WSink(bus: Stream[AxiLite4W], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends StreamSink(bus, clockDomain, queueOccupancyLimit) {
  var flowPercent: Int = 100

  override def sample(): Transaction = {
    AxiLite4WPkg(bus.data.toBigInt, bus.strb.toBigInt)
  }

  override def streamPause(): Boolean = nextInt(100) >= (flowPercent - 1)

  def setFlowPercent(flowPercent: Int) = {
    assert(flowPercent > 0 && flowPercent <= 100, s"flowPercent should be [1:100]")
    log.info(s"set flow percent :$flowPercent")
    this.flowPercent = flowPercent
  }
}

