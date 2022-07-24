package tb.driver

import spinal.core._
import tb.SimLog

abstract class BusDriver[T <: Data](bus: T, clockDomain: ClockDomain) extends Driver {

  val log = new SimLog(bus.getDisplayName())

  def init(): Unit

}
