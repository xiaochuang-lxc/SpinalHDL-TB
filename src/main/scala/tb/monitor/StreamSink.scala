package tb.monitor

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import tb.{Event, Transaction}

abstract class StreamSink[T <: Data](bus: Stream[T], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends BusMonitor(bus, clockDomain) {
  val active = Event()

  def streamPause(): Boolean

  def full = recvQ.size == queueOccupancyLimit

  def idle = !active.fired & recvQ.isEmpty

  def sample(): Transaction

  def getTrasaction[T <: Transaction] = {
    recvQ.dequeue().asInstanceOf[T]
  }

  override def init(): Unit = {
    bus.ready #= false
  }

  override def monitorRecv(): Unit = {
    while (true) {
      if (bus.valid.toBoolean) {
        active.set()
        if (bus.ready.toBoolean) {
          recv(sample())
        }
      } else {
        active.clear()
      }
      bus.ready #= !streamPause() && !full
      clockDomain.waitSampling()
    }
  }
}
