package tb.stream

import spinal.core.sim._
import tb.SimLog

import scala.util.Random._

object streamTest extends App {
  SimConfig.withFstWave.compile(dut()).doSim { dut =>
    val log = new SimLog("DUT")
    val pkgInDrvInst = pkgInDrv(dut.io.pkgIn, dut.clockDomain)
    val pkgOutMonInst = pkgOutMon(dut.io.pkgOut, dut.clockDomain, 32)
    dut.clockDomain.forkStimulus(10)
    pkgInDrvInst.init()
    pkgOutMonInst.init()
    dut.clockDomain.waitSampling()
    pkgInDrvInst.start()
    pkgOutMonInst.start()
    for (index <- 0 until 16) {
      pkgInDrvInst.send(pkgIn(nextInt(64), nextInt(64)))
    }
    waitUntil(pkgInDrvInst.idle)
    log.info("all data send done")
    waitUntil(pkgOutMonInst.idle)
    log.info("all data recv done")
    assert(pkgOutMonInst.recvQ.size == 16)
  }
}
