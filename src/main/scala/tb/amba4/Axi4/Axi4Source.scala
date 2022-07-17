package tb.amba4.Axi4

import spinal.core.sim._
import spinal.core.{ClockDomain, Data}
import spinal.lib.Stream
import spinal.lib.bus.amba4.axi._
import tb.Transaction
import tb.driver.StreamSource

import scala.util.Random.nextInt

abstract class Axi4Source[T <: Data](bus: Stream[T], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends StreamSource(bus, clockDomain, queueOccupancyLimit) {
  var flowPercent: Int = 100

  def setFlowPercent(flowPercent: Int) = {
    assert(flowPercent > 0 && flowPercent <= 100, s"flowPercent should be [1:100]")
    log.info(s"set flow percent :$flowPercent")
    this.flowPercent = flowPercent
  }
}

case class Axi4AxSource[T <: Axi4Ax](ax: Stream[T], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends Axi4Source(ax, clockDomain, queueOccupancyLimit) {
  val config = ax.config

  override def streamPause(): Boolean = nextInt(100) > (flowPercent - 1)

  override def sendData[T <: Transaction](transaction: T): Unit = {
    val cmd = transaction.asInstanceOf[Axi4AxPkg]
    ax.addr #= cmd.addr
    if (config.useId) ax.id #= cmd.id
    if (config.useRegion) ax.region #= cmd.region
    if (config.useLen) ax.len #= cmd.len
    if (config.useSize) ax.size #= cmd.size
    if (config.useBurst) ax.burst #= cmd.burst
    if (config.useLock) ax.lock #= cmd.lock
    if (config.useCache) ax.cache #= cmd.cache
    if (config.useQos) ax.qos #= cmd.qos
    if (config.useAwUser) ax.user #= cmd.user
    if (config.useProt) ax.prot #= cmd.prot
  }
}

case class Axi4WSource(w: Stream[Axi4W], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends Axi4Source(w, clockDomain, queueOccupancyLimit) {
  val config = w.config
  private var pkgBasePauseEnable = false
  var newPkgFlag = true

  def setPkgBasePause() = {
    pkgBasePauseEnable = true
  }

  def setStreamBasePause() = {
    pkgBasePauseEnable = false
  }


  override def streamPause(): Boolean = {
    if (pkgBasePauseEnable && !newPkgFlag) {
      false
    } else {
      nextInt(100) > (flowPercent - 1)
    }
  }

  override def sendData[T <: Transaction](transaction: T): Unit = {
    val pkg = transaction.asInstanceOf[Axi4WPkg]
    w.data #= pkg.data
    if (config.useStrb) w.strb #= pkg.strb
    if (config.useWUser) w.user #= pkg.user
    if (config.useLast) w.last #= pkg.last
    newPkgFlag = pkg.last
  }
}

case class Axi4BSource(b: Stream[Axi4B], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends Axi4Source(b, clockDomain, queueOccupancyLimit) {
  override def streamPause(): Boolean = nextInt(100) > (flowPercent - 1)

  override def sendData[T <: Transaction](transaction: T): Unit = {
    val pkg = transaction.asInstanceOf[Axi4BPkg]
    if (b.config.useResp) b.resp #= pkg.resp
    if (b.config.useId) b.id #= pkg.id
    if (b.config.useBUser) b.user #= pkg.user
  }
}

case class Axi4RSource(r: Stream[Axi4R], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends Axi4Source(r, clockDomain, queueOccupancyLimit) {
  val config = r.config
  private var pkgBasePauseEnable = false
  var newPkgFlag = true

  def setPkgBasePause() = {
    pkgBasePauseEnable = true
  }

  def setStreamBasePause() = {
    pkgBasePauseEnable = false
  }

  override def streamPause(): Boolean = {
    if (pkgBasePauseEnable && !newPkgFlag) {
      false
    } else {
      nextInt(100) > (flowPercent - 1)
    }
  }

  override def sendData[T <: Transaction](transaction: T): Unit = {
    val pkg = transaction.asInstanceOf[Axi4RPkg]
    r.data #= pkg.data
    if (config.useId) r.id #= pkg.id
    if (config.useResp) r.resp #= pkg.resp
    if (config.useLast) r.last #= pkg.last
    if (config.useRUser) r.user #= pkg.user
    newPkgFlag = pkg.last
  }
}
