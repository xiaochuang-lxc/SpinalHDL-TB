package tb.stream

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import tb.Transaction
import tb.monitor.StreamSink

case class sum(data: Int) extends Transaction

case class pkgOutMon(bus: Stream[UInt], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends StreamSink(bus, clockDomain, queueOccupancyLimit) {
  override def sample(): Transaction = {
    sum(bus.payload.toInt)
  }

  override def streamPause(): Boolean = scala.util.Random.nextBoolean()
}
