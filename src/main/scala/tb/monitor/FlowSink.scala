package tb.monitor

import spinal.core._
import spinal.core.sim._
import spinal.lib.Flow
import tb.{Event, Transaction}

abstract case class FlowSink[T <: Data](bus: Flow[T], clockDomain: ClockDomain, callback: (Transaction) => Unit = null, event: Event = null) extends BusMonitor(bus, clockDomain, callback, event) {
  val active = Event()

  def idle(): Boolean = !active.fired & recvQ.isEmpty

  def sample(): Transaction

  def getTransaction[T <: Transaction] = {
    waitUntil(!recvQ.isEmpty)
    recvQ.dequeue().asInstanceOf[T]
  }

  override def init(): Unit = {
    recvQ.clear()
    callbackArray.clear()
    if (callback != null)
      callbackArray.append(callback)
  }

  override def stop(): Unit = {
    if (monitorThread != null)
      monitorThread.terminate()
    recvQ.clear()
    active.clear()
  }

  override def monitorRecv(): Unit = {
    while (true) {
      if (bus.valid.toBoolean) {
        active.set()
        recv(sample())
      } else {
        active.clear()
      }
      clockDomain.waitSampling()
    }
  }
}
