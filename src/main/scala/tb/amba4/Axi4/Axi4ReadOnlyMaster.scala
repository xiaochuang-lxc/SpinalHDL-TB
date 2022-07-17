package tb.amba4.Axi4

import spinal.core.sim.{fork, simFailure, waitUntil}
import spinal.core.{ClockDomain, log2Up}
import spinal.lib.Stream
import spinal.lib.bus.amba4.axi.{Axi4Ar, Axi4Config, Axi4R}
import tb.Utils.BigInt2ByteArray
import tb.memory.Region
import tb.{Event, Transaction}

import scala.collection.mutable.{ArrayBuffer, Queue}

case class Axi4ReadCmd(addr: BigInt, length: Int, event: Event, config: Axi4Config, kwargs: Map[String, BigInt]) {

  def generatePkg(): Array[(Axi4AxPkg, Axi4ReadRespCmd)] = {
    val defaultSize = log2Up(config.bytePerWord)

    val pkgBuffer = Array[(Axi4AxPkg, Axi4ReadRespCmd)]().toBuffer

    val id = if (config.useId) kwargs.getOrElse("id", BigInt(0)) else BigInt(0)
    val region = if (config.useRegion) kwargs.getOrElse("region", BigInt(0)).toInt else 0
    val size = if (config.useSize) kwargs.getOrElse("size", BigInt(defaultSize)).toInt else 0
    val burst = if (config.useBurst) kwargs.getOrElse("burst", BigInt(1)).toInt else 0
    val lock = if (config.useLock) kwargs.getOrElse("lock", BigInt(0)).toInt else 0
    val cache = if (config.useCache) kwargs.getOrElse("cache", BigInt(0)).toInt else 0
    val qos = if (config.useQos) kwargs.getOrElse("qos", BigInt(0)).toInt else 0
    val prot = if (config.useProt) kwargs.getOrElse("prot", BigInt(2)).toInt else 0
    val aruser = if (config.useWUser) kwargs.getOrElse("awuser", BigInt(0)) else BigInt(0)

    val addrAlignOffset = addr.toInt & (config.bytePerWord - 1)
    val addr4KAlignOffset = addr.toInt & (4096 - 1)
    val maxCyclePerCmd = 4096 / config.bytePerWord
    var cycles = (addrAlignOffset + length + config.bytePerWord - 1) / config.bytePerWord
    val cmdNum = (addr4KAlignOffset / config.bytePerWord + cycles + maxCyclePerCmd - 1) / maxCyclePerCmd
    val firstCmdDataCycleMax = (4096 - addr4KAlignOffset + config.bytePerWord - 1) / config.bytePerWord

    for (index <- 0 until cmdNum) {
      val dataCycles = scala.math.min(cycles, if (index == 0) firstCmdDataCycleMax else maxCyclePerCmd)
      val arCmd = Axi4AxPkg(
        addr = if (index == 0) addr else (addr - addr4KAlignOffset) + 4096 * index,
        id = id,
        region = region,
        len = dataCycles - 1,
        size = size,
        burst = burst,
        lock = lock,
        cache = cache,
        qos = qos,
        user = aruser,
        prot = prot
      )
      pkgBuffer += ((arCmd, Axi4ReadRespCmd(arCmd, length, event, index == (cmdNum - 1), addrAlignOffset)))
      cycles = cycles - dataCycles
    }
    pkgBuffer.toArray
  }
}

case class Axi4ReadRespCmd(arCmd: Axi4AxPkg, length: Int, event: Event, triggerEvent: Boolean, addrAlignOffset: Int)

case class Axi4ReadOnlyMaster(ar: Stream[Axi4Ar], r: Stream[Axi4R], clockDomain: ClockDomain, maxPkgPending: Int = 8) extends Region(BigInt(1) << ar.config.addressWidth, 0) {
  val config = ar.config
  val readCmdQueue = Queue[Axi4ReadCmd]()
  val respCmdQueuArray = Array.fill(if (config.idWidth > 0) 1 << config.idWidth else 1)(Queue[Axi4ReadRespCmd]())
  val recvBytesBufferArray = Array.fill(if (config.idWidth > 0) 1 << config.idWidth else 1)(ArrayBuffer[Byte]())

  val arSource = Axi4AxSource(ar, clockDomain, maxPkgPending)
  val rSink = Axi4RSink(r, clockDomain, 4096 / config.bytePerWord * maxPkgPending)

  def init() = {
    arSource.init()
    rSink.init()
    rSink.addCallback(respProcess)
  }

  def start() = {
    arSource.start()
    rSink.start()
    fork(readProcess())
  }

  private def readProcess() = {
    while (true) {
      waitUntil(!readCmdQueue.isEmpty)
      val cmd = readCmdQueue.dequeue()
      val readPkgs = cmd.generatePkg()
      for ((arCmd, respCmd) <- readPkgs) {
        arSource.send(arCmd)
        arSource.log.info(s"send a read cmd ${arCmd.toString}")
        respCmdQueuArray(arCmd.id.toInt).enqueue(respCmd)
      }
    }
  }

  private def respProcess(resp: Transaction) = {
    val recvPkg = resp.asInstanceOf[Axi4RPkg]
    assert(!respCmdQueuArray(recvPkg.id.toInt).isEmpty, rSink.log.error(s"get a id=${recvPkg.id} resp from bus but no write cmd"))

    recvBytesBufferArray(recvPkg.id.toInt) ++= BigInt2ByteArray(recvPkg.data, config.bytePerWord)
    if (recvPkg.last) {
      val respCmd = respCmdQueuArray(recvPkg.id.toInt).dequeue()
      if (!recvPkg.isOkay) {
        rSink.log.warning(s"${respCmd.toString} get a ${recvPkg.showResp()} response")
      }
      if (respCmd.triggerEvent) {
        val readBytes = recvBytesBufferArray(recvPkg.id.toInt).toArray.slice(respCmd.addrAlignOffset, respCmd.length + respCmd.addrAlignOffset)
        recvBytesBufferArray(recvPkg.id.toInt).clear()
        respCmd.event.set(readBytes)
      }
    }
  }

  def idle = arSource.idle && rSink.idle && readCmdQueue.isEmpty && respCmdQueuArray.map(_.isEmpty).reduce(_ & _)

  def setFlowPercent(arFlowPercent: Int, rFowPercent: Int) = {
    arSource.setFlowPercent(arFlowPercent)
    rSink.setFlowPercent(rFowPercent)
  }

  def setPkgBasePause() = {
    rSink.setPkgBasePause()
  }

  def setStreamBasePause() = {
    rSink.setStreamBasePause()
  }

  override def readPrivate(address: BigInt, length: Int, kwargs: Map[String, BigInt]): Array[Byte] = {
    val readEvent = initRead(address, length, kwargs)
    readEvent.trigger()
    readEvent.getData[Array[Byte]]()
  }

  def initRead(address: BigInt, length: Int, kwargs: Map[String, BigInt]): Event = {
    val readEvent = Event()
    val readCmd = Axi4ReadCmd(address, length, readEvent, config, if (kwargs != null) kwargs else Map("id" -> 0))
    readCmdQueue.enqueue(readCmd)
    readEvent
  }

  override def writePrivate(address: BigInt, data: Array[Byte], kwargs: Map[String, BigInt]): Unit = {
    simFailure(s"Axi4ReadOnly not support write operation")
  }
}
