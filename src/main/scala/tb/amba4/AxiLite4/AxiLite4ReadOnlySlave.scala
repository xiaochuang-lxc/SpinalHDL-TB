package tb.amba4.AxiLite4

import spinal.core.sim.fork
import spinal.core.{ClockDomain, log2Up}
import spinal.lib.Stream
import spinal.lib.bus.amba4.axilite.{AxiLite4Ax, AxiLite4R}
import spinal.sim.SimThread
import tb.Utils.ByteArray2BigInt
import tb.memory.Region

case class AxiLite4ReadOnlySlave(ar: Stream[AxiLite4Ax], r: Stream[AxiLite4R], clockDomain: ClockDomain, target: Region, queueOccupancyLimit: Int = 8) {
  val config = ar.config
  val addressAlignMask = (BigInt(1) << log2Up(config.bytePerWord)) - 1
  val arMon = AxiLite4AxSink(ar, clockDomain, queueOccupancyLimit)
  val rDrv = AxiLite4RSource(r, clockDomain, queueOccupancyLimit)
  var readProcThrd: SimThread = null

  def init() = {
    arMon.init()
    rDrv.init()
  }

  def start() = {
    arMon.start()
    rDrv.start()
    readProcThrd = fork(readProcess())
  }

  def stop() = {
    if (readProcThrd != null) readProcThrd.terminate()
    arMon.stop()
    rDrv.stop()
  }

  def idel = arMon.idle && rDrv.idle

  def setFlowPercent(arFlowPercent: Int, rflowPercent: Int) = {
    arMon.setFlowPercent(arFlowPercent)
    rDrv.setFlowPercent(rflowPercent)
  }

  private def readProcess() = {
    while (true) {
      val arPkg = arMon.getItem[AxiLite4AxPkg]
      val readAddrOffset = (arPkg.addr & addressAlignMask).toInt
      val readLen = (config.bytePerWord - readAddrOffset)
      arMon.log.info(s"get a read cmd: addr:${arPkg.addr},length:${readLen}")
      val readData = target.read(arPkg.addr, readLen)
      val readAckData = Array.fill(readAddrOffset)(0.toByte) ++ readData
      rDrv.send(AxiLite4RPkg(ByteArray2BigInt(readAckData), 0))
    }
  }
}
