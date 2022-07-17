package tb.stream

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import tb.Transaction
import tb.driver.StreamSource

case class pkgIn(data0: Int, data1: Int) extends Transaction

case class pkgInDrv(bus: Stream[pkgInPort], clockDomain: ClockDomain) extends StreamSource(bus, clockDomain) {
  override def sendData[T <: Transaction](transaction: T): Unit = {
    val pkg = transaction.asInstanceOf[pkgIn]
    bus.data0 #= pkg.data0
    bus.data1 #= pkg.data1
  }

  override def streamPause(): Boolean = scala.util.Random.nextBoolean()
}
