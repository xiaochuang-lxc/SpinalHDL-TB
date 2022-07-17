package tb.amba4.Axi4

import spinal.core.sim.{fork, simFailure, waitUntil}
import spinal.core.{ClockDomain, log2Up}
import spinal.lib.Stream
import spinal.lib.bus.amba4.axi.{Axi4Aw, Axi4B, Axi4Config, Axi4W}
import tb.Utils.{BooleanList2BigInt, ByteArray2BigInt}
import tb.memory.Region
import tb.{Event, Transaction}

import scala.collection.mutable.Queue

case class Axi4WriteCmd(addr: BigInt, data: Array[Byte], event: Event, config: Axi4Config, kwargs: Map[String, BigInt]) {
  def generatePkg(): Array[(Axi4AxPkg, Array[Axi4WPkg], Axi4WriteRespCmd)] = {

    val defaultSize = log2Up(config.bytePerWord)

    val pkgBuffer = Array[(Axi4AxPkg, Array[Axi4WPkg], Axi4WriteRespCmd)]().toBuffer

    val id = if (config.useId) kwargs.getOrElse("id", BigInt(0)) else BigInt(0)
    val region = if (config.useRegion) kwargs.getOrElse("region", BigInt(0)).toInt else 0
    val size = if (config.useSize) kwargs.getOrElse("size", BigInt(defaultSize)).toInt else 0
    val burst = if (config.useBurst) kwargs.getOrElse("burst", BigInt(1)).toInt else 0
    val lock = if (config.useLock) kwargs.getOrElse("lock", BigInt(0)).toInt else 0
    val cache = if (config.useCache) kwargs.getOrElse("cache", BigInt(0)).toInt else 0
    val qos = if (config.useQos) kwargs.getOrElse("qos", BigInt(0)).toInt else 0
    val prot = if (config.useProt) kwargs.getOrElse("prot", BigInt(2)).toInt else 0
    val awuser = if (config.useWUser) kwargs.getOrElse("awuser", BigInt(0)) else BigInt(0)
    val wuser = if (config.useWUser) kwargs.getOrElse("wuser", BigInt(0)) else BigInt(0)

    val addrAlignOffset = addr.toInt & (config.bytePerWord - 1)
    val addr4KAlignOffset = addr.toInt & (4096 - 1)
    val maxCyclePerCmd = 4096 / config.bytePerWord
    val cycles = (addrAlignOffset + data.length + config.bytePerWord - 1) / config.bytePerWord
    val lastCyclByte = (data.length + addrAlignOffset) & (config.bytePerWord - 1)
    val dataTmp = Array.fill(addrAlignOffset)(0.toByte) ++ data ++ Array.fill(config.bytePerWord - lastCyclByte)(0.toByte)
    val strbTmp = Array.fill(addrAlignOffset)(false) ++ Array.fill(data.length)(true) ++ Array.fill(config.bytePerWord - lastCyclByte)(false)
    val cmdNum = (addr4KAlignOffset / config.bytePerWord + cycles + maxCyclePerCmd - 1) / maxCyclePerCmd
    val firstCmdDataCycleMax = (4096 - addr4KAlignOffset + config.bytePerWord - 1) / config.bytePerWord

    val wdataBuffer = Array[BigInt]().toBuffer
    val strbBuffer = Array[BigInt]().toBuffer
    for (index <- 0 until cycles) {
      wdataBuffer += ByteArray2BigInt(dataTmp.slice(index * config.bytePerWord, (index + 1) * config.bytePerWord))
      strbBuffer += BooleanList2BigInt(strbTmp.slice(index * config.bytePerWord, (index + 1) * config.bytePerWord))
    }

    for (index <- 0 until cmdNum) {
      val wPkgBuffer = Array[Axi4WPkg]().toBuffer
      val dataCycles = scala.math.min(wdataBuffer.length, if (index == 0) firstCmdDataCycleMax else maxCyclePerCmd)
      val awCmd = Axi4AxPkg(
        addr = if (index == 0) addr else (addr - addr4KAlignOffset) + 4096 * index,
        id = id,
        region = region,
        len = dataCycles - 1,
        size = size,
        burst = burst,
        lock = lock,
        cache = cache,
        qos = qos,
        user = awuser,
        prot = prot
      )
      val wCmds = wdataBuffer.slice(0, dataCycles).zipWithIndex.map(info => {
        Axi4WPkg(info._1, strbBuffer(info._2), wuser, info._2 == (dataCycles - 1))
      })
      pkgBuffer += ((awCmd, wCmds.toArray, Axi4WriteRespCmd(awCmd, event, index == (cmdNum - 1))))
      wdataBuffer.remove(0, dataCycles)
      strbBuffer.remove(0, dataCycles)
    }
    pkgBuffer.toArray
  }
}

case class Axi4WriteRespCmd(axi4AxPkg: Axi4AxPkg, event: Event, triggerEvent: Boolean)

case class Axi4WriteOnlyMaster(aw: Stream[Axi4Aw], w: Stream[Axi4W], b: Stream[Axi4B], clockDomain: ClockDomain, maxPkgPending: Int = 8) extends Region(BigInt(1) << aw.config.addressWidth, 0) {
  val config = aw.config
  val awSource = Axi4AxSource(aw, clockDomain, maxPkgPending)
  val wSource = Axi4WSource(w, clockDomain, 4096 / config.bytePerWord * maxPkgPending)
  val bSink = Axi4BSink(b, clockDomain, maxPkgPending)
  val respCmdQueuArray = Array.fill(if (config.idWidth > 0) 1 << config.idWidth else 1)(Queue[Axi4WriteRespCmd]())
  val writeCmdQueue = Queue[Axi4WriteCmd]()

  def init() = {
    awSource.init()
    wSource.init()
    bSink.init()
    bSink.addCallback(respProcess)
  }

  def start() = {
    awSource.start()
    wSource.start()
    bSink.start()
    fork(writeProcess)
  }

  private def writeProcess() = {
    while (true) {
      waitUntil(!writeCmdQueue.isEmpty)
      val cmd = writeCmdQueue.dequeue()
      val writePkgs = cmd.generatePkg()
      for ((awPkg, wPkg, respPkg) <- writePkgs) {
        awSource.send(awPkg)
        awSource.log.info(s"send a write cmd:${awPkg.toString}")
        wPkg.foreach(wSource.send(_))
        respCmdQueuArray(awPkg.id.toInt).enqueue(respPkg)
      }
    }
  }

  private def respProcess(resp: Transaction) = {
    val result = resp.asInstanceOf[Axi4BPkg]
    assert(!respCmdQueuArray(result.id.toInt).isEmpty, bSink.log.error(s"get a id=${result.id} resp from bus but no write cmd"))
    val respCmd = respCmdQueuArray(result.id.toInt).dequeue()
    if (!result.isOkay) {
      bSink.log.warning(s"${respCmd.toString} get a ${result.showResp()} response")
    }
    if (respCmd.triggerEvent)
      respCmd.event.set()
  }

  def setFlowPercent(awFlowPercent: Int, wflowPercent: Int, bflowPercent: Int) = {
    awSource.setFlowPercent(awFlowPercent)
    wSource.setFlowPercent(wflowPercent)
    bSink.setFlowPercent(bflowPercent)
  }

  def setPkgBasePause() = {
    wSource.setPkgBasePause()
  }

  def setStreamBasePause() = {
    wSource.setStreamBasePause()
  }

  def idle = awSource.idle && wSource.idle && bSink.idle && writeCmdQueue.isEmpty && respCmdQueuArray.map(_.isEmpty).reduce(_ & _)

  override def writePrivate(address: BigInt, data: Array[Byte], kwargs: Map[String, BigInt]): Unit = {
    val writeEvent = initWrite(address, data, kwargs)
    writeEvent.trigger()
  }

  def initWrite(address: BigInt, data: Array[Byte], kwargs: Map[String, BigInt]): Event = {
    val writeEvent = Event()
    val writeCmd = Axi4WriteCmd(address, data, writeEvent, config, if (kwargs != null) kwargs else Map("id" -> 0))
    writeCmdQueue.enqueue(writeCmd)
    writeEvent
  }

  override def readPrivate(address: BigInt, length: Int, kwargs: Map[String, BigInt]): Array[Byte] = {
    simFailure(s"Axi4WriteOnly not support read operation")
  }
}
