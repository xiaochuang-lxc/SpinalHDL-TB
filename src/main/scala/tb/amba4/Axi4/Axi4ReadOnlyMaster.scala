package tb.amba4.Axi4

import spinal.core.sim.{fork, simFailure, waitUntil}
import spinal.core.{ClockDomain, log2Up}
import spinal.lib.Stream
import spinal.lib.bus.amba4.axi.{Axi4Ar, Axi4Config, Axi4R}
import spinal.sim.SimThread
import tb.Utils.BigInt2ByteArray
import tb.memory.Region
import tb.{Event, Transaction}

import scala.collection.mutable.{ArrayBuffer, Queue}
import scala.math.min

case class Axi4ReadCmd(addr: BigInt, length: Int, event: Event, config: Axi4Config, kwargs: Map[String, BigInt]) {

  val defaultSize = log2Up(config.bytePerWord)
  val id = if (config.useId) kwargs.getOrElse("id", BigInt(0)) else BigInt(0)
  val region = if (config.useRegion) kwargs.getOrElse("region", BigInt(0)).toInt else 0
  val size = if (config.useSize) kwargs.getOrElse("size", BigInt(defaultSize)).toInt else defaultSize
  val burst = if (config.useBurst) kwargs.getOrElse("burst", BigInt(1)).toInt else 0
  val lock = if (config.useLock) kwargs.getOrElse("lock", BigInt(0)).toInt else 0
  val cache = if (config.useCache) kwargs.getOrElse("cache", BigInt(0)).toInt else 0
  val qos = if (config.useQos) kwargs.getOrElse("qos", BigInt(0)).toInt else 0
  val prot = if (config.useProt) kwargs.getOrElse("prot", BigInt(2)).toInt else 0
  val aruser = if (config.useWUser) kwargs.getOrElse("aruser", BigInt(0)) else BigInt(0)
  val transferBytesPerCycle = 1 << size //每拍传输的字节数

  def generatePkg(): Array[(Axi4AxPkg, Axi4ReadRespCmd)] = {
    val pkgBuffer = Array[(Axi4AxPkg, Axi4ReadRespCmd)]().toBuffer

    var lengthLeft = length
    var sendAddr = addr
    while (lengthLeft > 0) {
      val allowTransferMax = transferBytesPerCycle * 256-(sendAddr.toInt&(transferBytesPerCycle-1))
      val allowTransferBytes = min(min(4096 - (sendAddr.toInt & 4095), allowTransferMax), lengthLeft) //允许传输的字节数
      val addrAlignOffset = sendAddr.toInt & (transferBytesPerCycle - 1) //首拍地址偏移
      val cycles = (addrAlignOffset + allowTransferBytes + transferBytesPerCycle - 1) / transferBytesPerCycle //数据传输需要的cycle数
      val arCmd = Axi4AxPkg(
        addr = sendAddr,
        id = id,
        region = region,
        len = cycles - 1,
        size = size,
        burst = burst,
        lock = lock,
        cache = cache,
        qos = qos,
        user = aruser,
        prot = prot
      )
      pkgBuffer += ((arCmd, Axi4ReadRespCmd(arCmd, length, event, lengthLeft == allowTransferBytes, addr.toInt & (transferBytesPerCycle - 1))))
      sendAddr += allowTransferBytes
      lengthLeft -= allowTransferBytes
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
  val recvBigIntBufferArray = Array.fill(if (config.idWidth > 0) 1 << config.idWidth else 1)(ArrayBuffer[BigInt]())

  val arSource = Axi4AxSource(ar, clockDomain, maxPkgPending)
  val rSink = Axi4RSink(r, clockDomain, 4096 / config.bytePerWord * maxPkgPending)

  var readProcThrd: SimThread = null

  def init() = {
    arSource.init()
    rSink.init()
    rSink.addCallback(respProcess)
  }

  def start() = {
    arSource.start()
    rSink.start()
    readProcThrd = fork(readProcess())
  }

  def stop(): Unit = {
    readProcThrd.terminate()
    arSource.stop()
    rSink.stop()
    readCmdQueue.clear()
    respCmdQueuArray.foreach(_.clear())
    recvBytesBufferArray.foreach(_.clear())
    recvBigIntBufferArray.foreach(_.clear())
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

  private def filterReadData(arCmd: Axi4AxPkg, readData: ArrayBuffer[BigInt]): Array[Byte] = {
    val transferPerBytes = 1 << arCmd.size //每拍传输的字节数
    val alignTransferOffset = arCmd.addr.toInt & (transferPerBytes - 1)
    val startAddressAligned = (arCmd.addr.toInt - alignTransferOffset) & (config.bytePerWord - 1)
    val dataSliceSelected = for (index <- 0 until readData.length) yield (startAddressAligned + index * transferPerBytes) & (config.bytePerWord - 1)
    val readDataByteArray = readData.map(BigInt2ByteArray(_, config.bytePerWord))
    (readDataByteArray, dataSliceSelected).zipped.map((rdata, offset) => rdata.slice(offset, offset + transferPerBytes)).reduce(_ ++ _)
  }

  private def respProcess(resp: Transaction) = {
    val recvPkg = resp.asInstanceOf[Axi4RPkg]
    assert(!respCmdQueuArray(recvPkg.id.toInt).isEmpty, rSink.log.error(s"get a id=${recvPkg.id} resp from bus but no write cmd"))
    recvBigIntBufferArray(recvPkg.id.toInt) += recvPkg.data
    if (recvPkg.last) {
      val respCmd = respCmdQueuArray(recvPkg.id.toInt).dequeue()
      recvBytesBufferArray(recvPkg.id.toInt) ++= filterReadData(arCmd = respCmd.arCmd, recvBigIntBufferArray(recvPkg.id.toInt))
      recvBigIntBufferArray(recvPkg.id.toInt).clear()
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
