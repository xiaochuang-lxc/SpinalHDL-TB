package tb.amba4.Axi4

import spinal.core.sim._
import spinal.core.{ClockDomain, Data,log2Up}
import spinal.lib.Stream
import spinal.lib.bus.amba4.axi.{Axi4Ax, Axi4B, Axi4R, Axi4W}
import tb.Transaction
import tb.monitor.StreamSink

import scala.util.Random.nextInt

abstract class Axi4Sink[T <: Data](bus: Stream[T], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends StreamSink(bus, clockDomain, queueOccupancyLimit) {
  var flowPercent: Int = 100

  def setFlowPercent(flowPercent: Int) = {
    assert(flowPercent > 0 && flowPercent <= 100, s"flowPercent should be [1:100]")
    log.info(s"set flow percent :$flowPercent")
    this.flowPercent = flowPercent
  }
}

case class Axi4AxSink[T <: Axi4Ax](ax: Stream[T], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends Axi4Sink(ax, clockDomain, queueOccupancyLimit) {
  val config = ax.config
  val maxSize=log2Up(config.bytePerWord)

  override def streamPause(): Boolean = nextInt(100) > (flowPercent - 1)

  override def sample(): Transaction = {
    Axi4AxPkg(
      ax.addr.toBigInt,
      if (config.useId) ax.id.toInt else 0,
      if (config.useRegion) ax.region.toInt else 0,
      if (config.useLen) ax.len.toInt else 0,
      if (config.useSize) ax.size.toInt else maxSize,
      if (config.useBurst) ax.burst.toInt else 0,
      if (config.useLock) ax.lock.toInt else 0,
      if (config.useCache) ax.cache.toInt else 0,
      if (config.useQos) ax.qos.toInt else 0,
      if (config.useAwUser) ax.user.toBigInt else 0,
      if (config.useProt) ax.prot.toInt else 0
    )
  }
}

case class Axi4WSink(w: Stream[Axi4W], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends Axi4Sink(w, clockDomain, queueOccupancyLimit) {
  val config = w.config
  private var pkgBasePauseEnable = false
  var newPkgFlag = true

  override def streamPause(): Boolean = {
    if (pkgBasePauseEnable && !newPkgFlag) {
      false
    } else {
      nextInt(100) > (flowPercent - 1)
    }
  }

  def setPkgBasePause() = {
    pkgBasePauseEnable = true
  }

  def setStreamBasePause() = {
    pkgBasePauseEnable = false
  }

  override def sample(): Transaction = {
    val lastValue = if (config.useLast) w.last.toBoolean else true
    newPkgFlag = lastValue
    Axi4WPkg(
      w.data.toBigInt,
      if (config.useStrb) w.strb.toBigInt else (BigInt(1) << config.bytePerWord) - 1,
      if (config.useWUser) w.user.toBigInt else 0,
      lastValue
    )
  }
}

case class Axi4BSink(b: Stream[Axi4B], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends Axi4Sink(b, clockDomain, queueOccupancyLimit) {
  val config = b.config

  override def streamPause(): Boolean = nextInt(100) > (flowPercent - 1)

  override def sample(): Transaction = {
    Axi4BPkg(
      if (config.useId) b.id.toBigInt else 0,
      if (config.useResp) b.resp.toInt else 0,
      if (config.useBUser) b.user.toBigInt else BigInt(0)
    )
  }
}

case class Axi4RSink(r: Stream[Axi4R], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends Axi4Sink(r, clockDomain, queueOccupancyLimit) {
  val config = r.config
  private var pkgBasePauseEnable = false
  var newPkgFlag = true

  override def streamPause(): Boolean = {
    if (pkgBasePauseEnable && !newPkgFlag) {
      false
    } else {
      nextInt(100) > (flowPercent - 1)
    }
  }

  def setPkgBasePause() = {
    pkgBasePauseEnable = true
  }

  def setStreamBasePause() = {
    pkgBasePauseEnable = false
  }

  override def sample(): Transaction = {
    val lastValue = if (config.useLast) r.last.toBoolean else true
    newPkgFlag = lastValue
    Axi4RPkg(
      r.data.toBigInt,
      if (config.useId) r.id.toInt else 0,
      if (config.useResp) r.resp.toInt else 0,
      lastValue,
      if (config.useRUser) r.user.toBigInt else 0
    )
  }

}
