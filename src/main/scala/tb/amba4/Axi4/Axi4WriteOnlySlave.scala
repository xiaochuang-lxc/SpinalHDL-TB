package tb.amba4.Axi4

import spinal.core._
import spinal.core.sim.fork
import spinal.lib.Stream
import spinal.lib.bus.amba4.axi.{Axi4Aw, Axi4B, Axi4W}
import spinal.lib.tools.BigIntToListBoolean
import spinal.sim.SimThread
import tb.Utils.BigInt2ByteArray
import tb.memory.Region

import scala.collection.mutable.ArrayBuffer

case class Axi4WriteOnlySlave(aw: Stream[Axi4Aw], w: Stream[Axi4W], b: Stream[Axi4B], clockDomain: ClockDomain, maxPkgPending: Int = 8, target: Region) {
  val config = aw.config
  val awSink = Axi4AxSink(aw, clockDomain, maxPkgPending)
  val wSink = Axi4WSink(w, clockDomain, 4096 / config.bytePerWord * maxPkgPending)
  val bSource = Axi4BSource(b, clockDomain, maxPkgPending)
  val writeDataBuffer = ArrayBuffer[Byte]()
  val writeStrbBuffer = ArrayBuffer[Boolean]()
  var writeProcThrd: SimThread = null

  def init() = {
    awSink.init()
    wSink.init()
    bSource.init()
    //wSink.addCallback(writeProcess)
  }

  def start(): Unit = {
    awSink.start()
    wSink.start()
    bSource.start()
    writeProcThrd = fork(writeProcess)
  }

  def stop() = {
    writeProcThrd.terminate()
    awSink.stop()
    wSink.stop()
    bSource.stop()
    writeDataBuffer.clear()
    writeDataBuffer.clear()
  }

  def setFlowPercent(awFlowPercent: Int, wflowPercent: Int, bflowPercent: Int) = {
    awSink.setFlowPercent(awFlowPercent)
    wSink.setFlowPercent(wflowPercent)
    bSource.setFlowPercent(bflowPercent)
  }

  def setPkgBasePause() = {
    wSink.setPkgBasePause()
  }

  def setStreamBasePause() = {
    wSink.setStreamBasePause()
  }

  def idle = awSink.idle && wSink.idle && bSource.idle && writeDataBuffer.isEmpty && writeStrbBuffer.isEmpty


  private def writeProcess = {
    var wChBeatCnt=0
    while (true) {
      val axi4WPkg = wSink.getItem[Axi4WPkg] // transaction.asInstanceOf[Axi4WPkg]
      writeDataBuffer ++= BigInt2ByteArray(axi4WPkg.data, config.bytePerWord)
      writeStrbBuffer ++= BigIntToListBoolean(axi4WPkg.strb, config.bytePerWord bits)
      wChBeatCnt+=1
      if (axi4WPkg.last) {
        val writeCmd = awSink.getItem[Axi4AxPkg]
        writeCmd.validCheck()
        awSink.log.info(s"get a write cmd:${writeCmd.toString}")
        val writeData = (writeDataBuffer, writeStrbBuffer).zipped.toArray.filter(_._2).map(_._1)
        if(wChBeatCnt!=(writeCmd.len+1)){
          wSink.log.error(s"len mismatch:write data length:${writeData.length}, the axi4Aw Cmd:${writeCmd.toString}")
        }
        if(writeData.length>(4096-(writeCmd.addr.toInt&4095))){
          wSink.log.error(s"corss 4K error:write data length${writeData.length}, the axi4Aw Cmd:${writeCmd.toString}")
        }
        target.write(writeCmd.addr, writeData, axi4WPkg.generatekwargsMap(writeCmd))
        wSink.log.info(s"write addr:${writeCmd.addr},length:${writeData.length}")
        bSource.send(Axi4BPkg(writeCmd.id, 0, writeCmd.user))
        writeDataBuffer.clear()
        writeStrbBuffer.clear()
        wChBeatCnt=0
      }
    }
  }
  /*
  private def writeProcess(transaction: Transaction): Unit = {
    val axi4WPkg = transaction.asInstanceOf[Axi4WPkg]
    writeDataBuffer ++= BigInt2ByteArray(axi4WPkg.data, config.bytePerWord)
    writeStrbBuffer ++= BigIntToListBoolean(axi4WPkg.strb, config.bytePerWord bits)
    if (axi4WPkg.last) {
      val writeCmd = awSink.getItem[Axi4AxPkg]
      awSink.log.info(s"get a write cmd:${writeCmd.toString}")
      val writeData = (writeDataBuffer, writeStrbBuffer).zipped.toArray.filter(_._2).map(_._1)
      val addrAlignOffset = writeCmd.addr.toInt & (config.bytePerWord - 1)
      val addr4KAlignOffset = addrAlignOffset & (4096 - 1)
      val cycles = (addrAlignOffset + writeData.length + config.bytePerWord - 1) / config.bytePerWord
      if (cycles != (writeCmd.len + 1)) {
        wSink.log.error(s"len mismatch:write data length:${writeData.length}, the axi4Aw Cmd:${writeCmd.toString}")
      }
      if (writeData.length > (4096 - addr4KAlignOffset)) {
        if (cycles != (writeCmd.len - 1)) {
          wSink.log.error(s"cross 4K error:write data length:${writeData.length}, the axi4Aw Cmd:${writeCmd.toString}")
        }
      }
      target.write(writeCmd.addr, writeData, axi4WPkg.generatekwargsMap(writeCmd))
      wSink.log.info(s"write addr:${writeCmd.addr},length:${writeData.length}")
      bSource.send(Axi4BPkg(writeCmd.id, 0, writeCmd.user))
      writeDataBuffer.clear()
      writeStrbBuffer.clear()
    }
  }

   */


}
