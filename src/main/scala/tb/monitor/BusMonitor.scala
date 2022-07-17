package tb.monitor

import spinal.core._
import tb.{Event, SimLog, Transaction}

abstract class BusMonitor[T <: Data](bus: T, clockDomain: ClockDomain, callback: (Transaction) => Unit = null, event: Event = null) extends Monitor(callback, event) {
  val log = new SimLog(bus.getDisplayName())

  def init(): Unit
}
