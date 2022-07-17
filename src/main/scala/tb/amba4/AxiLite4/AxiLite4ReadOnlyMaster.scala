package tb.amba4.AxiLite4

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import tb.Utils.BigInt2ByteArray
import tb.memory._
import tb.{Event, Transaction}

import scala.collection.mutable.{ArrayBuffer, Queue}

case class AxiLiteReadRespCmd(addr: BigInt, length: Int, prot: BigInt, event: Event, triggerEvent: Boolean, addrAlignOffset: Int)

case class AxiLiteReadCmd(addr: BigInt, length: Int, event: Event, kwargs: Map[String, BigInt]) {
  val prot = if (kwargs != null) kwargs.getOrElse("prot", BigInt(2)) else BigInt(2)

  def generatePkg(config: AxiLite4Config): Array[(AxiLite4AxPkg, AxiLiteReadRespCmd)] = {
    val pkgs = Array[(AxiLite4AxPkg, AxiLiteReadRespCmd)]().toBuffer
    val addrAlignOffset = (addr % config.bytePerWord).toInt
    val cycles = (addrAlignOffset + length + config.bytePerWord - 1) / config.bytePerWord
    for (index <- 0 until cycles) {
      val addrToSend = if (index == 0) addr else (addr - addrAlignOffset) + index * config.bytePerWord
      pkgs += (
        (AxiLite4AxPkg(addrToSend, prot),
          AxiLiteReadRespCmd(addrToSend, length, prot, event, index == (cycles - 1), addrAlignOffset)
        )
        )
    }
    pkgs.toArray
  }
}

case class AxiLite4ReadOnlyMaster(ar: Stream[AxiLite4Ax], r: Stream[AxiLite4R], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends Region(BigInt(1) << ar.config.addressWidth, 0) {
  val readCmdQueue = Queue[AxiLiteReadCmd]()
  val respCmdQueue = Queue[AxiLiteReadRespCmd]()
  val recvBytes = ArrayBuffer[Byte]()
  val arDrv = AxiLite4AxSource(ar, clockDomain, queueOccupancyLimit)
  val rMon = AxiLite4RSink(r, clockDomain, queueOccupancyLimit)

  private def readCmdProc() = {
    while (true) {
      waitUntil(!readCmdQueue.isEmpty)
      val readCmd = readCmdQueue.dequeue()
      val cmdPkgs = readCmd.generatePkg(ar.config)
      for ((arCmd, respCmd) <- cmdPkgs) {
        respCmdQueue.enqueue(respCmd)
        arDrv.send(arCmd)
        arDrv.log.info(s"send a read cmd: addr=${arCmd.addr}")
      }
    }
  }

  private def readRespProc(resp: Transaction) = {
    assert(!respCmdQueue.isEmpty, s"$simTime() get a resp but no read cmd")
    val recvPkg = resp.asInstanceOf[AxiLite4RPkg]
    val respCmd = respCmdQueue.dequeue()
    if (!recvPkg.isOkay) {
      rMon.log.warning(f"read cmd addr:${respCmd.addr}%x\t prot:${respCmd.prot} get a ${recvPkg.showResp()} response")
    }
    recvBytes ++= BigInt2ByteArray(recvPkg.data, r.config.bytePerWord)
    if (respCmd.triggerEvent) {
      val readBytes = recvBytes.toArray.slice(respCmd.addrAlignOffset, respCmd.length + respCmd.addrAlignOffset)
      recvBytes.clear()
      respCmd.event.set(readBytes)
    }
  }

  def init() = {
    arDrv.init()
    rMon.init()
    rMon.addCallback(readRespProc)
  }

  def idle = arDrv.idle && rMon.idle && readCmdQueue.isEmpty && respCmdQueue.isEmpty

  def start() = {
    arDrv.start()
    rMon.start()
    fork(readCmdProc())
  }

  def setFlowPercent(arFlowPercent: Int, rflowPercent: Int) = {
    arDrv.setFlowPercent(arFlowPercent)
    rMon.setFlowPercent(rflowPercent)
  }

  override def writePrivate(address: BigInt, data: Array[Byte], kwargs: Map[String, BigInt]): Unit = {
    simFailure(s"AxiLite4ReadOnly not support write operation")
  }

  override def readPrivate(address: BigInt, length: Int, kwargs: Map[String, BigInt]): Array[Byte] = {
    val readEvent = Event()
    val readCmd = AxiLiteReadCmd(address, length, readEvent, kwargs)
    readCmdQueue.enqueue(readCmd)
    readEvent.trigger()
    readEvent.data.asInstanceOf[Array[Byte]]
  }

  def initRead(address: BigInt, length: Int, prot: BigInt): Event = {
    val readEvent = Event()
    val readCmd = AxiLiteReadCmd(address, length, readEvent, Map("prot" -> prot))
    readCmdQueue.enqueue(readCmd)
    readEvent
  }
}
