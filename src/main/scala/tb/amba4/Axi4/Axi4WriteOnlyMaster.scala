package tb.amba4.Axi4

import spinal.core.sim.{fork, simFailure, waitUntil}
import spinal.core.{ClockDomain, log2Up}
import spinal.lib.Stream
import spinal.lib.bus.amba4.axi.{Axi4Aw, Axi4B, Axi4Config, Axi4W}
import spinal.sim.SimThread
import tb.Utils.{BooleanList2BigInt, ByteArray2BigInt}
import tb.memory.Region
import tb.{Event, Transaction}

import scala.collection.mutable.Queue
import scala.math.min

case class Axi4WriteCmd(addr: BigInt, data: Array[Byte], event: Event, config: Axi4Config, kwargs: Map[String, BigInt]) {

  val defaultSize = log2Up(config.bytePerWord)
  val id = if (config.useId) kwargs.getOrElse("id", BigInt(0)) else BigInt(0)
  val region = if (config.useRegion) kwargs.getOrElse("region", BigInt(0)).toInt else 0
  val size = if (config.useSize) kwargs.getOrElse("size", BigInt(defaultSize)).toInt else defaultSize
  val burst = if (config.useBurst) kwargs.getOrElse("burst", BigInt(1)).toInt else 0
  val lock = if (config.useLock) kwargs.getOrElse("lock", BigInt(0)).toInt else 0
  val cache = if (config.useCache) kwargs.getOrElse("cache", BigInt(0)).toInt else 0
  val qos = if (config.useQos) kwargs.getOrElse("qos", BigInt(0)).toInt else 0
  val prot = if (config.useProt) kwargs.getOrElse("prot", BigInt(2)).toInt else 0
  val awuser = if (config.useWUser) kwargs.getOrElse("awuser", BigInt(0)) else BigInt(0)
  val wuser = if (config.useWUser) kwargs.getOrElse("wuser", BigInt(0)) else BigInt(0)

  val transferBytesPerCycle = 1 << size //每拍传输的字节数


  /**
   * 单个指令封装
   *
   * @param sendAddr  待发送地址
   * @param sendData  待发送数据
   * @param fireEvent 是否触发Axi4WriteRespCmd Event
   * @return
   */
  private def generateCmd(sendAddr: BigInt, sendData: Array[Byte], fireEvent: Boolean): (Axi4AxPkg, Array[Axi4WPkg], Axi4WriteRespCmd) = {
    val addrAlignOffset = sendAddr.toInt & (transferBytesPerCycle - 1) //首拍地址偏移
    val cycles = (addrAlignOffset + sendData.length + transferBytesPerCycle - 1) / transferBytesPerCycle //数据传输需要的cycle数
    val lastCycleByteNum = (addrAlignOffset + sendData.length) & (transferBytesPerCycle - 1) //最后一拍传输的字节数
    val dataTmp = (Array.fill(addrAlignOffset)(0.toByte) ++ sendData ++ Array.fill((transferBytesPerCycle-lastCycleByteNum)&(transferBytesPerCycle-1))(0.toByte)) //待发送数据拼接
    val strbTmp = Array.fill(addrAlignOffset)(false) ++ Array.fill(sendData.length)(true) ++ Array.fill((transferBytesPerCycle - lastCycleByteNum) & (transferBytesPerCycle - 1))(false)

    val axi4WBuffer = Array[Axi4WPkg]().toBuffer
    val awCmd = Axi4AxPkg(
      addr = sendAddr,
      id = id,
      region = region,
      len = cycles - 1,
      size = size,
      burst = burst,
      lock = lock,
      cache = cache,
      qos = qos,
      user = awuser,
      prot = prot
    )
    for (index <- 0 until cycles) {
      val offset = if (index == 0) (sendAddr.toInt - addrAlignOffset) & (config.bytePerWord - 1) else ((sendAddr - addrAlignOffset).toInt + index * transferBytesPerCycle) & (config.bytePerWord - 1)
      val dataSlice = Array.fill(offset)(0.toByte) ++ dataTmp.slice(index * transferBytesPerCycle, (index + 1) * transferBytesPerCycle) ++ Array.fill(config.bytePerWord - offset - transferBytesPerCycle)(0.toByte)
      val strbSlice = Array.fill(offset)(false) ++ strbTmp.slice(index * transferBytesPerCycle, (index + 1) * transferBytesPerCycle) ++ Array.fill(config.bytePerWord - offset - transferBytesPerCycle)(false)
      axi4WBuffer.append(Axi4WPkg(
        data = ByteArray2BigInt(dataSlice),
        strb = BooleanList2BigInt(strbSlice),
        user = wuser,
        last = index == cycles - 1
      ))
    }
    (awCmd, axi4WBuffer.toArray, Axi4WriteRespCmd(awCmd, event, fireEvent))
  }

  def generatePkg(): Array[(Axi4AxPkg, Array[Axi4WPkg], Axi4WriteRespCmd)] = {

    val pkgBuffer = Array[(Axi4AxPkg, Array[Axi4WPkg], Axi4WriteRespCmd)]().toBuffer

    var lengthLeft = data.length
    var sendAddr = addr
    val dataToSend = data.toBuffer
    while (lengthLeft > 0) {
      val allowTransferMax = transferBytesPerCycle * 256-(sendAddr.toInt&(transferBytesPerCycle-1))
      val allowTransferBytes = min(min(4096 - (sendAddr.toInt & 4095), allowTransferMax), lengthLeft) //允许传输的字节数
      val sendData = dataToSend.slice(0, allowTransferBytes) //待发送的数据
      dataToSend.remove(0, allowTransferBytes)
      pkgBuffer += generateCmd(sendAddr, sendData.toArray, allowTransferBytes == lengthLeft)
      sendAddr += allowTransferBytes
      lengthLeft -= allowTransferBytes
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
  var writeProcThrd: SimThread = null

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
    writeProcThrd = fork(writeProcess)
  }

  def stop() = {
    writeProcThrd.terminate()
    awSource.stop()
    wSource.stop()
    bSink.stop()
    respCmdQueuArray.foreach(_.clear())
    writeCmdQueue.clear()
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
