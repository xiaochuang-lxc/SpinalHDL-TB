package tb.memory

import org.scalatest.FunSuite

class MemoryRegionTest extends FunSuite {
  val memoryRegionInst = new MemoryRegion(4096, 0)

  test("normal read and write match") {
    //read and write bytes
    val writeBytes = Array(1, 2, 3, 4, 5).map(_.toByte)
    memoryRegionInst.write(0, writeBytes)
    val readBytes = memoryRegionInst.read(0, writeBytes.length)
    assert(writeBytes.toBuffer == readBytes.toBuffer, s"writeBytes:$writeBytes\nreadBytes:$readBytes")
    //dwrods read and write
    val writeDwords = Array(4, 5, 6, 7, 8).map(BigInt(_))
    memoryRegionInst.writeDwords(0x10, writeDwords)
    val readDwords = memoryRegionInst.readDwords(0x10, writeDwords.length)
    assert(writeDwords.toBuffer == readDwords.toBuffer, s"writeBytes:$writeDwords\nreadBytes:$readDwords")
  }

  test("window read and write") {
    val window = memoryRegionInst.createWindow(0x10, 1024)
    val writeDwords = Array(4, 5, 6, 7, 8).map(BigInt(_))
    window.writeDwords(0, writeDwords)
    val readDwords = memoryRegionInst.readDwords(0x10, writeDwords.length)
    assert(writeDwords.toBuffer == readDwords.toBuffer, s"writeBytes:$writeDwords\nreadBytes:$readDwords")
    memoryRegionInst.writeDwords(0x20, writeDwords)
    val readDowrds1 = window.readDwords(0x10, writeDwords.length)
    assert(writeDwords.toBuffer == readDowrds1.toBuffer, s"writeBytes:$writeDwords\nreadBytes:$readDowrds1")
  }

  test("window pool read and write") {
    val windowPool = memoryRegionInst.createWindowPool(0x100, 1024)
    val window = windowPool.allocWindow(0x100)
    val window1 = windowPool.allocWindow(0x100)

    def windowTest(window: Window, address: BigInt) = {
      val writeDwords = Array(4, 5, 6, 7, 8).map(BigInt(_))
      window.writeDwords(address, writeDwords)
      val readDwords0 = window.readDwords(address, writeDwords.length)
      assert(writeDwords.toBuffer == readDwords0.toBuffer, s"writeBytes:$writeDwords\nreadBytes:$readDwords0")
      val readDwords1 = windowPool.readDwords(window.base - memoryRegionInst.base + address, writeDwords.length)
      assert(writeDwords.toBuffer == readDwords1.toBuffer, s"writeBytes:$writeDwords\nreadBytes:$readDwords1")
      val readDwords2 = memoryRegionInst.readDwords(windowPool.base + window.base - memoryRegionInst.base + address, writeDwords.length)
      assert(writeDwords.toBuffer == readDwords2.toBuffer, s"writeBytes:$writeDwords\nreadBytes:$readDwords2")
    }

    windowTest(window, 0x20)
    windowTest(window1, 0x30)
    windowPool.freeWindow(window)
    windowPool.freeWindow(window1)
    assert(windowPool.allocator.allocations.isEmpty)
  }
}
