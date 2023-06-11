package tb.amba4.Axi4

import org.scalatest.FunSuite
import spinal.core.sim._
import spinal.lib.bus.amba4.axi.Axi4Config
import tb.SimLog
import tb.memory.MemoryRegion

import scala.util.Random.nextInt

case class Axi4Test() extends FunSuite {
  val config = Axi4Config(
    addressWidth = 20,
    dataWidth = 512,
    idWidth = 4,
    useId = true,
    useRegion = false,
    useBurst = true,
    useLock = false,
    useCache = false,
    useSize = true,
    useQos = false,
    useLen = true,
    useLast = true,
    useResp = true,
    useProt = true,
    useStrb = true,
    arUserWidth = 32,
    awUserWidth = 32,
    rUserWidth = 32,
    wUserWidth = 32,
    bUserWidth = 32
  )
  val dutCompiled = SimConfig.withFstWave.compile(Axi4Dut(config))
  val memRegion = new MemoryRegion(size = 1 << config.addressWidth)
  val log = SimLog("dut")

  def generateData(length: Int): Array[Byte] = {
    Array.fill(length)(nextInt(256)).map(_.toByte)
  }

  test("normal test:write address 0 with 1~4096 length") {
    dutCompiled.doSim { dut =>
      val axi4MasterDrv = Axi4Master(dut.io.input, dut.clockDomain, 16)
      val axi4SlaveDrv = Axi4Slave(dut.io.output, dut.clockDomain, 8, memRegion)
      axi4SlaveDrv.init()
      axi4MasterDrv.init()
      axi4MasterDrv.setFlowPercent(50, 50, 50, 50, 50)
      axi4SlaveDrv.setFlowPercent(50, 50, 50, 50, 50)
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling(10)
      axi4SlaveDrv.start()
      axi4MasterDrv.start()
      for (index <- 1 to 4096) {
        val writeData = generateData(index)
        log.info(s"send a package :address=0\tlength:${index}")
        axi4MasterDrv.write(0, writeData, Map("awuser" -> BigInt(0x0f0f0f0f), "id" -> BigInt(index % 8)))
        val readData = axi4MasterDrv.read(0x0, index, Map("aruser" -> BigInt(0x0f0f0f0f), "id" -> BigInt(index % 8)))
        val readDataBack = memRegion.read(0x0, index)
        assert(readDataBack.toBuffer == writeData.toBuffer, s"readDataBack and writeData Mismatch")
        assert(readData.toBuffer == writeData.toBuffer, s"readDataBack and writeData Mismatch")
      }
      axi4SlaveDrv.setPkgBasePause()
      axi4MasterDrv.setPkgBasePause()
      for (index <- 1 to 4096) {
        val writeData = generateData(index)
        axi4MasterDrv.write(0, writeData, Map("awuser" -> BigInt(0x0f0f0f0f), "id" -> BigInt(index % 8)))
        val readData = axi4MasterDrv.read(0x0, index, Map("aruser" -> BigInt(0x0f0f0f0f), "id" -> BigInt(index % 8)))
        val readDataBack = memRegion.read(0x0, index)
        assert(readDataBack.toBuffer == writeData.toBuffer, s"readDataBack and writeData Mismatch")
        assert(readData.toBuffer == writeData.toBuffer, s"readDataBack and writeData Mismatch")
      }
      axi4SlaveDrv.stop()
      axi4MasterDrv.stop()
    }
  }

  test("cross 4K test:address:0~4095 length:4096,8192,12288") {

    dutCompiled.doSim { dut =>
      val axi4MasterDrv = Axi4Master(dut.io.input, dut.clockDomain, 16)
      val axi4SlaveDrv = Axi4Slave(dut.io.output, dut.clockDomain, 8, memRegion)
      axi4SlaveDrv.init()
      axi4MasterDrv.init()
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling(10)
      axi4SlaveDrv.start()
      axi4MasterDrv.start()
      for {
        addr <- 4095 to 0 by -1;
        length <- 4096 to 12288 by 4096
      } {
        log.info(s"start test:addr=$addr\t length:$length")
        val writeData = generateData(length)
        axi4MasterDrv.write(addr, writeData, Map("awuser" -> BigInt(0x0f0f0f0f), "id" -> BigInt(addr % 8)))
        val readData = axi4MasterDrv.read(addr, length, Map("aruser" -> BigInt(0x0f0f0f0f), "id" -> BigInt(addr % 8)))
        val readDataBack = memRegion.read(addr, length)
        assert(readDataBack.toBuffer == writeData.toBuffer, s"readDataBack and writeData Mismatch")
        assert(readData.toBuffer == writeData.toBuffer, s"readDataBack and writeData Mismatch")
      }
      axi4SlaveDrv.stop()
      axi4MasterDrv.stop()
    }
  }

  test("initRead and initWrite Test") {
    dutCompiled.doSim { dut =>
      val axi4MasterDrv = Axi4Master(dut.io.input, dut.clockDomain, 16)
      val axi4SlaveDrv = Axi4Slave(dut.io.output, dut.clockDomain, 8, memRegion)
      axi4SlaveDrv.init()
      axi4MasterDrv.init()
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling(10)
      axi4SlaveDrv.start()
      axi4MasterDrv.start()
      axi4MasterDrv.setFlowPercent(50, 50, 50, 50, 50)
      axi4SlaveDrv.setFlowPercent(50, 50, 50, 50, 50)
      val writeData = for (index <- 1 to 16) yield generateData(index * 1024)
      val writeDataEvent = for (index <- 1 to 16) yield axi4MasterDrv.initWrite(index * 16384 + 1, writeData(index - 1), Map("awuser" -> BigInt(0x0f0f0f0f), "id" -> BigInt(index % 8)))
      writeDataEvent.foreach(_.trigger())
      log.info(s"Write Done")
      val writeDataBack = for (index <- 1 to 16) yield memRegion.read(index * 16384 + 1, index * 1024)
      (writeData, writeDataBack).zipped.foreach((wdata, wdataBack) => assert(wdata.toBuffer == wdataBack.toBuffer))
      val readDataEvent = for (index <- 1 to 16) yield axi4MasterDrv.initRead(index * 16384 + 1, index * 1024, Map("awuser" -> BigInt(0x0f0f0f0f), "id" -> BigInt(index % 8)))
      readDataEvent.foreach(_.trigger())
      log.info(s"read date done")
      val readData = readDataEvent.map(_.getData[Array[Byte]]())
      (writeData, readData).zipped.foreach((wdata, rdata) => assert(wdata.toBuffer == rdata.toBuffer))
      axi4SlaveDrv.stop()
      axi4MasterDrv.stop()
    }
  }
}
