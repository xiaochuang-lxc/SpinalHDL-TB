package tb.amba4.Axi4

import spinal.core._
import spinal.core.sim._
import spinal.lib.Stream
import spinal.lib.bus.amba4.axi.{Axi4Ar, Axi4R}
import tb.Utils.ByteArray2BigInt
import tb.memory.Region

case class Axi4ReadOnlySlave(ar: Stream[Axi4Ar], r: Stream[Axi4R], clockDomain: ClockDomain, maxPkgPending: Int = 8, target: Region) {
  val config = ar.config
  val arSink = Axi4AxSink(ar, clockDomain, maxPkgPending)
  val rSource = Axi4RSource(r, clockDomain, 4096 / config.bytePerWord * maxPkgPending)

  def init() = {
    arSink.init()
    rSource.init()
    //arSink.addCallback(readProcess)
  }

  def start() = {
    arSink.start()
    rSource.start()
    fork(readProcess)
  }

  def idle = arSink.idle && rSource.idle

  def setFlowPercent(arFlowPercent: Int, rFowPercent: Int) = {
    arSink.setFlowPercent(arFlowPercent)
    rSource.setFlowPercent(rFowPercent)
  }

  def setPkgBasePause() = {
    rSource.setPkgBasePause()
  }

  def setStreamBasePause() = {
    rSource.setStreamBasePause()
  }

  private def readProcess() = {
    while (true) {
      val readCmd = arSink.getItem[Axi4AxPkg]
      arSink.log.info(s"get a read cmd ${readCmd.toString}")
      val addrAlignOffset = readCmd.addr.toInt & (config.bytePerWord - 1)
      val addr4KAlignOffset = addrAlignOffset & (4096 - 1)
      val readLength = (readCmd.len + 1) * config.bytePerWord - addrAlignOffset
      if (readLength > (4096 - addr4KAlignOffset)) {
        arSink.log.error(s"cross 4K error: ${readCmd.toString}")
      }
      val readDataTmp = Array.fill(addrAlignOffset)(0.toByte) ++ target.read(readCmd.addr, readLength, readCmd.generatekwargsMap())
      for (index <- 0 to readCmd.len) {
        rSource.send(
          Axi4RPkg(
            data = ByteArray2BigInt(readDataTmp.slice(index * config.bytePerWord, (index + 1) * config.bytePerWord)),
            id = readCmd.id,
            last = index == readCmd.len,
            user = readCmd.user,
            resp = 0
          )
        )
      }
    }
  }

  /*private def readProcess(transaction: Transaction) = {
    val readCmd = transaction.asInstanceOf[Axi4AxPkg]
    arSink.log.info(s"get a read cmd ${readCmd.toString}")
    val addrAlignOffset = readCmd.addr.toInt & (config.bytePerWord - 1)
    val addr4KAlignOffset = addrAlignOffset & (4096 - 1)
    val readLength = (readCmd.len + 1) * config.bytePerWord - addrAlignOffset
    if (readLength > (4096 - addr4KAlignOffset)) {
      arSink.log.error(s"cross 4K error: ${readCmd.toString}")
    }
    val readDataTmp = Array.fill(addrAlignOffset)(0.toByte) ++ target.read(readCmd.addr, readLength, readCmd.generatekwargsMap())
    for (index <- 0 to readCmd.len) {
      rSource.send(
        Axi4RPkg(
          data = ByteArray2BigInt(readDataTmp.slice(index * config.bytePerWord, (index + 1) * config.bytePerWord)),
          id = readCmd.id,
          last = index == readCmd.len,
          user = readCmd.user,
          resp = 0
        )
      )
    }
  }*/
}
