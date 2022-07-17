package tb.memory

import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class AddressSpace(size: BigInt, base: BigInt = 0, parent: MemoryInterface = null) extends Region(size, base, parent) {
  val regionRegisterTable = ArrayBuffer[Region]() //base ,size, region

  def findRegions(address: BigInt, length: BigInt = 1): ArrayBuffer[Region] = {
    if (address < 0 || address >= size)
      simFailure(s"address:$address\t length:$length  out of range")
    if (length < 0) {
      simFailure(s"$length invalid length")
    }
    regionRegisterTable.filter(region => address < (region.base + region.size) && (region.base < (address + length))).sortBy(_.size)
  }

  def unRegisterRegion(region: Region) = {
    val regionMatchedIndex = regionRegisterTable.indexWhere(_ == region)
    if (regionMatchedIndex < 0)
      simFailure(s"$region is not found")
    regionRegisterTable.remove(regionMatchedIndex)
  }

  def registerRegion(region: Region, base: BigInt, size: BigInt) = {
    if (!findRegions(base, size).isEmpty) {
      simFailure(s"$region base:$base size:$size overlaps existing region")
    }
    region.parent = this
    region.base = base
    regionRegisterTable.append(region)
  }

  override def writePrivate(address: BigInt, data: Array[Byte], kwargs: Map[String, BigInt] = null): Unit = {
    var start = 0
    var length = data.length
    var addressTmp = address
    val regions = findRegions(address, length)
    if (regions.isEmpty)
      simFailure(s"address:$address\t is not Valid address")
    for (region <- regions) {
      val startAddr = addressTmp - region.base
      val writeLen = math.min((region.size - startAddr).toInt, length)
      region.write(startAddr, data.slice(start, start + writeLen), kwargs)
      addressTmp += writeLen
      start += writeLen
      length -= writeLen
    }
  }


  override def readPrivate(address: BigInt, length: Int, kwargs: Map[String, BigInt] = null): Array[Byte] = {
    val regions = findRegions(address, length)
    val rdata = Array[Byte]().toBuffer
    var addressTmp = address
    var lengthTmp = length
    if (regions.isEmpty)
      simFailure(s"address:$address\t is not Valid address")
    for (region <- regions) {
      val startAddr = addressTmp - region.base
      val readLen = math.min((region.size - startAddr).toInt, lengthTmp)
      rdata.appendAll(region.read(startAddr, readLen, kwargs))
      addressTmp += readLen
      lengthTmp -= readLen
    }
    rdata.toArray
  }

  def createRegionPool(base: BigInt, size: BigInt, minAlloc: Int = 1): MemoryRegionPool = {
    checkRange(base, size)
    val memRegionPoolInst = new MemoryRegionPool(size, base, minAlloc = minAlloc)
    registerRegion(memRegionPoolInst, base, size)
    memRegionPoolInst
  }
}
