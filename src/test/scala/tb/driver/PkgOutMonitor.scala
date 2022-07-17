package tb.driver

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import tb.monitor.BusMonitor
import tb.{Event, Transaction}

case class PkgOutMonitor(bus: Flow[UInt], clockDomain: ClockDomain, callback: (Transaction) => Unit = null, event: Event = null) extends BusMonitor(bus, clockDomain, callback, event) {
  override def init(): Unit = {
    addCallback(showResult)
  }

  def showResult(transaction: Transaction) = {
    val pkg = transaction.asInstanceOf[pkgOut]
    log.info(s"recv a pkg \t ${pkg.toString}")
  }

  override def monitorRecv(): Unit = {
    while (true) {
      if (bus.valid.toBoolean) {
        recv(pkgOut(bus.payload.toInt))
      }
      clockDomain.waitSampling()
    }
  }
}

