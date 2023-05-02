package tb.amba4.AxiLite4

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.Stream
import spinal.lib.bus.amba4.axilite.{AxiLite4Ax, AxiLite4B, AxiLite4W}
import spinal.sim.SimThread
import tb.memory.Region

case class AxiLite4WriteOnlySlave(aw: Stream[AxiLite4Ax], w: Stream[AxiLite4W], b: Stream[AxiLite4B], target: Region, clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) {
  val config = aw.config
  val awMon = AxiLite4AxSink(aw, clockDomain, queueOccupancyLimit)
  val wMon = AxiLite4WSink(w, clockDomain, queueOccupancyLimit)
  val bDrv = AxiLite4BSource(b, clockDomain, queueOccupancyLimit)
  var writeProcThrd: SimThread = null

  def init() = {
    awMon.init()
    wMon.init()
    bDrv.init()
  }

  def start() = {
    awMon.start()
    wMon.start()
    bDrv.start()
    writeProcThrd = fork(writeProcess())
  }

  def stop() = {
    if (writeProcThrd != null)
      writeProcThrd.terminate()
    awMon.stop()
    wMon.stop()
    bDrv.stop()
  }


  def idle = awMon.idle && wMon.idle && bDrv.idle

  def setFlowPercent(awFlowPercent: Int, wflowPercent: Int, bflowPercent: Int) = {
    awMon.setFlowPercent(awFlowPercent)
    wMon.setFlowPercent(wflowPercent)
    bDrv.setFlowPercent(bflowPercent)
  }

  private def writeProcess() = {
    while (true) {
      val awPkg = awMon.getItem[AxiLite4AxPkg]
      val wPkg = wMon.getItem[AxiLite4WPkg]
      val wdata = wPkg.getWriteCmd(awPkg.addr, config.bytePerWord)
      awMon.log.info(s"get a write cmd: addr:${awPkg.addr},length:${wdata.length}")
      target.write(awPkg.addr, wdata)
      bDrv.send(AxiLite4BPkg(0))
    }
  }
}
