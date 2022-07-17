package tb.memory

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.{ArrayBuffer, Map}

/**
 * memory alloc and free
 *
 * @param size     memory size
 * @param minAlloc min alloc size
 */
case class BuddyAllocator(size: BigInt, minAlloc: Int = 1) {
  val freeList = ArrayBuffer.fill(log2Up(size))(ArrayBuffer[BigInt]())
  freeList.append(ArrayBuffer[BigInt](0))
  val allocations = Map[BigInt, Int]()

  /**
   * alloc size bytes memory space
   *
   * @param size memory size
   * @return the start addr of the malloc region
   */
  def malloc(size: BigInt): BigInt = {
    if (size < 1 || size > this.size) {
      simFailure(s"size:${size} out of range")
    }
    val mallocSize = if (size > minAlloc) size else BigInt(minAlloc)
    var bucket = log2Up(mallocSize)
    val origBucket = bucket
    while ((bucket < freeList.size) && freeList(bucket).isEmpty) {
      bucket += 1
    }
    while (bucket > origBucket) {
      val block = freeList(bucket).remove(0)
      bucket -= 1
      freeList(bucket).append(block)
      freeList(bucket).append(block + (BigInt(1) << bucket))
    }
    if (freeList(bucket).isEmpty) {
      simFailure(s"size:${size} out of range")
    } else {
      val block = freeList(bucket).remove(0)
      allocations += (block -> bucket)
      return block
    }
  }

  /**
   * free memory region
   *
   * @param addr the base addr of memory region
   */
  def free(addr: BigInt) = {
    if (!allocations.contains(addr)) {
      simFailure(s"unknown allocation")
    }
    var bucket = allocations.remove(addr).get
    var matched: Boolean = false
    var addrTmp = addr

    for (index <- bucket until freeList.size if !matched) {
      val buddy = if (((addrTmp >> index) & 1) == 0) addrTmp + (BigInt(1) << index) else addrTmp - (BigInt(1) << index)
      if (freeList(index).contains(buddy)) {
        freeList(index).remove(freeList(index).indexOf(buddy))
        if (addrTmp > buddy)
          addrTmp = buddy
      } else {
        freeList(index).append(addrTmp)
        matched = true
      }
    }
    if (!matched) {
      simFailure(s"failed to free memory")
    }
  }
}