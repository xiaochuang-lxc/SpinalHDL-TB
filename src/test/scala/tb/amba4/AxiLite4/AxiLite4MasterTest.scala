package tb.amba4.AxiLite4

import org.scalatest.FunSuite
import spinal.core.sim._
import tb.{Event, SimLog}

class AxiLite4MasterTest extends FunSuite {
  val dutCompiled = SimConfig.withFstWave.compile(dut())

  test("normal read and write") {
    dutCompiled.doSim { dut =>

      val aliteDrv = AxiLite4Master(dut.io.alite, dut.clockDomain, 16)
      dut.clockDomain.forkStimulus(10)
      aliteDrv.init()
      dut.clockDomain.waitSampling(10)
      aliteDrv.setFlowPercent(50, 50, 50, 50, 50)
      aliteDrv.start()
      for (index <- 0 until 16) {
        aliteDrv.writeDWord(index * 4, index)
        assert(aliteDrv.readDword(index * 4) == index)
      }

      val writeData = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).map(_.toByte)
      aliteDrv.write(0x0, writeData)
      val readData = aliteDrv.read(0, writeData.length)
      assert(writeData.toBuffer == readData.toBuffer, s"write data:${writeData}\n readData:${readData}")
      aliteDrv.write(0x3, writeData)
      val readData1 = aliteDrv.read(3, writeData.length)
      assert(writeData.toBuffer == readData1.toBuffer, s"write data:${writeData}\n readData:${readData1}")
    }
  }

  import scala.collection.mutable.ArrayBuffer

  test("init read and write") {
    dutCompiled.doSim { dut =>
      val log = SimLog("dut")
      val aliteDrv = AxiLite4Master(dut.io.alite, dut.clockDomain, 16)
      dut.clockDomain.forkStimulus(10)
      aliteDrv.init()
      dut.clockDomain.waitSampling(10)
      aliteDrv.start()
      val writeArrayBuffer, readArrayBuffer = ArrayBuffer[Byte]()
      val writeEventBuffer, readEventBuffer = ArrayBuffer[Event]()
      //write operation
      log.info("start send write cmd")
      for (index <- 0 until 16) {
        val writeData = Array(index, index + 1, index + 2, index + 3).map(_.toByte)
        writeEventBuffer += aliteDrv.initWrite(index * 4, writeData, 2)
        writeArrayBuffer ++= writeData
      }
      log.info("send write cmd done")
      writeEventBuffer.foreach(_.trigger())
      log.info("write operation done")
      //read operation
      log.info("start send read cmd")
      for (index <- 0 until 16) {
        readEventBuffer += aliteDrv.initRead(index * 4, 4, 2)
      }
      log.info("send read cmd done")
      readEventBuffer.foreach(_.trigger())
      log.info("read operation done")
      readEventBuffer.foreach(readArrayBuffer ++= _.getData[Array[Byte]]())
      assert(writeArrayBuffer == readArrayBuffer)
    }
  }
}
