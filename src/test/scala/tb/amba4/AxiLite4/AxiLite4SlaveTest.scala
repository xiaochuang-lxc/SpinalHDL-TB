package tb.amba4.AxiLite4

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import tb.memory.MemoryRegion
import tb.{Event, SimLog}

case class testDut() extends Component {
  val io = new Bundle {
    val aliteIn = slave(AxiLite4(10, 32))
    val aliteOut = master(AxiLite4(10, 32))
    AxiLite4SpecRenamer(aliteIn)
    AxiLite4SpecRenamer(aliteOut)
  }
  noIoPrefix()
  io.aliteOut.aw <-/< io.aliteIn.aw
  io.aliteOut.w <-/< io.aliteIn.w
  io.aliteIn.b <-/< io.aliteOut.b
  io.aliteOut.ar <-/< io.aliteIn.ar
  io.aliteIn.r <-/< io.aliteOut.r
}

class AxiLite4SlaveTest extends FunSuite {
  val dutCompiled = SimConfig.withFstWave.compile(testDut())
  val memRegion = new MemoryRegion(1 << 10, 0)

  test("normal read and write") {
    dutCompiled.doSim { dut =>
      val aliteMasterDrv = AxiLite4Master(dut.io.aliteIn, dut.clockDomain, 16)
      val aliteSlaveMon = AxiLite4Slave(dut.io.aliteOut, dut.clockDomain, memRegion, 16)
      dut.clockDomain.forkStimulus(10)
      aliteMasterDrv.init()
      aliteSlaveMon.init()
      dut.clockDomain.waitSampling(10)
      aliteMasterDrv.setFlowPercent(50, 50, 50, 50, 50)
      aliteMasterDrv.start()
      aliteSlaveMon.start()
      for (index <- 0 until 16) {
        aliteMasterDrv.writeDWord(index * 4, index)
        assert(aliteMasterDrv.readDword(index * 4) == index)
      }

      val writeData = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).map(_.toByte)
      aliteMasterDrv.write(0x0, writeData)
      val readData = aliteMasterDrv.read(0, writeData.length)
      assert(writeData.toBuffer == readData.toBuffer, s"write data:${writeData}\n readData:${readData}")
      aliteMasterDrv.write(0x3, writeData)
      val readData1 = aliteMasterDrv.read(3, writeData.length)
      assert(writeData.toBuffer == readData1.toBuffer, s"write data:${writeData}\n readData:${readData1}")
    }
  }

  import scala.collection.mutable.ArrayBuffer

  test("init read and write") {
    dutCompiled.doSim { dut =>
      val log = SimLog("dut")
      val aliteMasterDrv = AxiLite4Master(dut.io.aliteIn, dut.clockDomain, 16)
      val aliteSlaveMon = AxiLite4Slave(dut.io.aliteOut, dut.clockDomain, memRegion, 16)
      dut.clockDomain.forkStimulus(10)
      aliteMasterDrv.init()
      aliteSlaveMon.init()
      dut.clockDomain.waitSampling(10)
      aliteMasterDrv.start()
      aliteSlaveMon.start()
      val writeArrayBuffer, readArrayBuffer = ArrayBuffer[Byte]()
      val writeEventBuffer, readEventBuffer = ArrayBuffer[Event]()
      //write operation
      log.info("start send write cmd")
      for (index <- 0 until 16) {
        val writeData = Array(index, index + 1, index + 2, index + 3).map(_.toByte)
        writeEventBuffer += aliteMasterDrv.initWrite(index * 4, writeData, 2)
        writeArrayBuffer ++= writeData
      }
      log.info("send write cmd done")
      writeEventBuffer.foreach(_.trigger())
      log.info("write operation done")
      //read operation
      log.info("start send read cmd")
      for (index <- 0 until 16) {
        readEventBuffer += aliteMasterDrv.initRead(index * 4, 4, 2)
      }
      log.info("send read cmd done")
      readEventBuffer.foreach(_.trigger())
      log.info("read operation done")
      readEventBuffer.foreach(readArrayBuffer ++= _.getData[Array[Byte]]())
      assert(writeArrayBuffer == readArrayBuffer)
    }
  }
}
