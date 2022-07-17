package tb.driver

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import tb.Transaction

import scala.collection.mutable.ArrayBuffer

case class PortInDriver(bus: Flow[dataPort], clockDomain: ClockDomain) extends BusDriver(bus, clockDomain) {
  val busName = bus.getName()
  val refResult = ArrayBuffer[pkgOut]()

  override def init(): Unit = {
    bus.flattenForeach(setBigInt(_, 0))
  }

  override def sendData[T <: Transaction](transaction: T): Unit = {
    val pkgData = transaction.asInstanceOf[PkgIn]
    refResult += pkgOut(pkgData.sum)
    bus.valid #= true
    bus.data0 #= pkgData.data0
    bus.data1 #= pkgData.data1
    clockDomain.waitSampling()
    log.info(s"send a Pkg:\t${pkgData.toString}")
    bus.valid #= false
  }
}
