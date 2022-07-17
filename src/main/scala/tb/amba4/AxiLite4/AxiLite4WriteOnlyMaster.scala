package tb.amba4.AxiLite4

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import tb.Utils.{BooleanList2BigInt, ByteArray2BigInt}
import tb.memory.Region
import tb.{Event, Transaction}

import scala.collection.mutable.Queue

case class AxiLite4WriteRespCmd(addr: BigInt, data: Array[Byte], prot: BigInt, event: Event, triggerEvent: Boolean)

case class AxiLite4WriteCmd(addr: BigInt, data: Array[Byte], event: Event, kwargs: Map[String, BigInt]) {
  val prot = if (kwargs != null) kwargs.getOrElse("prot", BigInt(2)) else BigInt(2)

  def generatePkg(config: AxiLite4Config): Array[(AxiLite4AxPkg, AxiLite4WPkg, AxiLite4WriteRespCmd)] = {
    val pkgs = Array[(AxiLite4AxPkg, AxiLite4WPkg, AxiLite4WriteRespCmd)]().toBuffer
    val addrAlignOffset = (addr % config.bytePerWord).toInt
    val cycles = (addrAlignOffset + data.length + config.bytePerWord - 1) / config.bytePerWord
    val lastCyclByte = (data.length + addrAlignOffset) % config.bytePerWord
    val dataTmp = Array.fill(addrAlignOffset)(0.toByte) ++ data ++ Array.fill(config.bytePerWord - lastCyclByte)(0.toByte)
    val strbTmp = Array.fill(addrAlignOffset)(false) ++ Array.fill(data.length)(true) ++ Array.fill(config.bytePerWord - lastCyclByte)(false)
    for (index <- 0 until cycles) {
      val dataToSend = ByteArray2BigInt(dataTmp.slice(index * config.bytePerWord, (index + 1) * config.bytePerWord))
      val strb = BooleanList2BigInt(strbTmp.slice(index * config.bytePerWord, (index + 1) * config.bytePerWord))
      val addrToSend = if (index == 0) addr else (addr - addrAlignOffset) + index * config.bytePerWord
      pkgs += (
        (AxiLite4AxPkg(addrToSend, prot),
          AxiLite4WPkg(dataToSend, strb),
          AxiLite4WriteRespCmd(dataToSend, data, prot, event, index == (cycles - 1))
        )
        )
    }
    pkgs.toArray
  }
}


case class AxiLite4WriteOnlyMaster(aw: Stream[AxiLite4Ax], w: Stream[AxiLite4W], b: Stream[AxiLite4B], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends Region(BigInt(1) << aw.config.addressWidth, 0) {
  val config = aw.config
  val writeCmdQueue = Queue[AxiLite4WriteCmd]()
  val respCmdQueue = Queue[AxiLite4WriteRespCmd]()
  val awDrv = AxiLite4AxSource(aw, clockDomain, queueOccupancyLimit)
  val wDrv = AxiLite4WSource(w, clockDomain, queueOccupancyLimit)
  val bMon = AxiLite4BSink(b, clockDomain, queueOccupancyLimit)

  def init() = {
    awDrv.init()
    wDrv.init()
    bMon.init()
    bMon.addCallback(respProcess)
  }

  def start() = {
    awDrv.start()
    wDrv.start()
    bMon.start()
    fork(writeProcess())
  }

  private def writeProcess() = {
    while (true) {
      waitUntil(!writeCmdQueue.isEmpty)
      val cmd = writeCmdQueue.dequeue()
      val writePkg = cmd.generatePkg(config)
      for ((axPkg, wPkg, bPkg) <- writePkg) {
        respCmdQueue.enqueue(bPkg)
        awDrv.send(axPkg)
        wDrv.send(wPkg)
        awDrv.log.info(s"send a write cmd: addr:${axPkg.addr}")
      }

    }
  }

  private def respProcess(resp: Transaction) = {
    val result = resp.asInstanceOf[AxiLite4BPkg]
    assert(!respCmdQueue.isEmpty, s"$simTime() get a resp but no write cmd")
    val respCmd = respCmdQueue.dequeue()
    if (!result.isOkay) {
      bMon.log.warning(s"addr :${respCmd.addr}\tport:${respCmd.prot}\t write data:${respCmd.data} get a ${result.showResp()} response")
    }
    if (respCmd.triggerEvent) {
      respCmd.event.set()
    }
  }

  def setFlowPercent(awFlowPercent: Int, wflowPercent: Int, bflowPercent: Int) = {
    awDrv.setFlowPercent(awFlowPercent)
    wDrv.setFlowPercent(wflowPercent)
    bMon.setFlowPercent(bflowPercent)
  }

  override def writePrivate(address: BigInt, data: Array[Byte], kwargs: Map[String, BigInt]): Unit = {
    val writeEvent = Event()
    val writeCmd = AxiLite4WriteCmd(address, data, writeEvent, kwargs)
    writeCmdQueue.enqueue(writeCmd)
    writeEvent.trigger()
  }

  def initWrite(address: BigInt, data: Array[Byte], prot: BigInt): Event = {
    val writeEvent = Event()
    val writeCmd = AxiLite4WriteCmd(address, data, writeEvent, Map("prot" -> prot))
    writeCmdQueue.enqueue(writeCmd)
    writeEvent
  }

  override def readPrivate(address: BigInt, length: Int, kwargs: Map[String, BigInt]): Array[Byte] = {
    simFailure(s"AxiLite4WriteOnly not support read operation")
  }

  def idle = awDrv.idle && wDrv.idle && bMon.idle && writeCmdQueue.isEmpty && respCmdQueue.isEmpty
}
