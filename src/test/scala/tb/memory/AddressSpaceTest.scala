package tb.memory

import org.scalatest.FunSuite

class AddressSpaceTest extends FunSuite {
  val addressSpaceInst = new AddressSpace(1024 * 128, 0)
  val memoryRegionPool = addressSpaceInst.createRegionPool(4096, 1024)
  val memoryRegionInst = new MemoryRegion(1024, 0)
  val memoryRegionInst1 = new MemoryRegion(1024)
  addressSpaceInst.registerRegion(memoryRegionInst, 0x10, memoryRegionInst.size)
  addressSpaceInst.registerRegion(memoryRegionInst1, 1040, memoryRegionInst1.size)


  test("Address Space normal read and write") {
    //read and write bytes
    val writeBytes = Array(1, 2, 3, 4, 5).map(_.toByte)
    addressSpaceInst.write(0x10, writeBytes)
    val readBytes = addressSpaceInst.read(0x10, writeBytes.length)
    assert(writeBytes.toBuffer == readBytes.toBuffer, s"writeBytes:$writeBytes\nreadBytes:$readBytes")
    val readBytes1 = memoryRegionInst.read(0x0, writeBytes.length)
    assert(writeBytes.toBuffer == readBytes1.toBuffer, s"writeBytes:$writeBytes\nreadBytes:$readBytes1")

    //dwrods read and write
    val writeDwords = Array(4, 5, 6, 7, 8).map(BigInt(_))
    addressSpaceInst.writeDwords(0x100, writeDwords)
    val readDwords = addressSpaceInst.readDords(0x100, writeDwords.length)
    assert(writeDwords.toBuffer == readDwords.toBuffer, s"writeBytes:$writeDwords\nreadBytes:$readDwords")
    val readDwords1 = memoryRegionInst.readDords(0xf0, writeBytes.length)
    assert(writeDwords.toBuffer == readDwords1.toBuffer, s"writeBytes:$writeDwords\nreadBytes:$readDwords1")
  }

  test("Address Space  region pool normal test") {
    val memoryRegion, memoryRegion1 = memoryRegionPool.allocRegion(256)
    val writeBytes = Array(1, 2, 3, 4, 5).map(_.toByte)
    memoryRegion1.write(0x10, writeBytes)
    val readBytes0 = memoryRegion1.read(0x10, writeBytes.length)
    val readBytes1 = memoryRegionPool.read(memoryRegion1.base + 0x10, writeBytes.length)
    val readBytes2 = addressSpaceInst.read(memoryRegionPool.base + memoryRegion1.base + 0x10, writeBytes.length)
    assert(writeBytes.toBuffer == readBytes0.toBuffer)
    assert(writeBytes.toBuffer == readBytes1.toBuffer)
    assert(writeBytes.toBuffer == readBytes2.toBuffer)
    memoryRegionPool.freeRegion(memoryRegion)
    memoryRegionPool.freeRegion(memoryRegion1)
    assert(memoryRegionPool.allocator.allocations.isEmpty)
  }

  test("Address Space read and write cross region") {
    val writeBytes = Array(1, 2, 3, 4, 5).map(_.toByte)
    addressSpaceInst.write(1038, writeBytes)
    val readBytes0 = addressSpaceInst.read(1038, writeBytes.length)
    assert(writeBytes.toBuffer == readBytes0.toBuffer)
  }
}
