package tb.memory

class MemoryRegion(size: Int, base: BigInt = 0, parent: MemoryInterface = null) extends Region(size, base, parent) {
  val memBuffer = Array.fill(size)(0.toByte)

  override def writePrivate(address: BigInt, data: Array[Byte], kwargs: Map[String, BigInt] = null): Unit = {
    Array.copy(data, 0, memBuffer, address.toInt, data.length)
  }

  override def readPrivate(address: BigInt, length: Int, kwargs: Map[String, BigInt] = null): Array[Byte] = {
    memBuffer.slice(address.toInt, length + address.toInt)
  }
}
