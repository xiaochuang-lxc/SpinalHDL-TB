package tb.memory

import spinal.core.sim._

class Window(size: BigInt, parent: MemoryInterface, base: BigInt = 0) extends MemoryInterface(size, base, parent) {
  override def readPrivate(address: BigInt, length: Int, kwargs: Map[String, BigInt] = null): Array[Byte] = {
    parent.read(getParentAddress(address), length, kwargs)
  }

  def getParentAddress(address: BigInt) = {
    if (address < 0 || address > this.size) {
      simFailure(s"address:$address out of range")
    }
    base + address
  }

  override def writePrivate(address: BigInt, data: Array[Byte], kwargs: Map[String, BigInt] = null): Unit = {
    parent.write(getParentAddress(address), data, kwargs)
  }

}
