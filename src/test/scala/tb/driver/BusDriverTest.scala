package tb.driver

import spinal.core.sim._
import tb.Scoreboard.Scoreboard
import tb.SimLog

object BusDriverTest extends App {
  SimConfig.withFstWave.compile(dut()).doSim { dut =>
    val log = new SimLog("DUT")
    val portInDriverInst = PortInDriver(dut.io.pkgIn, dut.clockDomain)
    val pkgOutMonitorInst = PkgOutMonitor(dut.io.pktOut, dut.clockDomain)
    val scoreboard = new Scoreboard()
    scoreboard.addInterfaces(pkgOutMonitorInst, portInDriverInst.refResult, 0, null)
    portInDriverInst.init()
    pkgOutMonitorInst.init()
    dut.clockDomain.forkStimulus(10000)
    dut.clockDomain.waitSampling()
    pkgOutMonitorInst.start()
    for (data <- 0 until (500000)) {
      portInDriverInst.drive(PkgIn(data % 64, (data + 2) % 64))
    }
    dut.clockDomain.waitSampling(10)

    log.info(s"send pkg cnt:${portInDriverInst.transactionCount}\npkg recv:${pkgOutMonitorInst.transactionCount}")
  }
}
