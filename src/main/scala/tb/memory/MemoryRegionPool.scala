package tb.memory

class MemoryRegionPool(size: BigInt, base: BigInt = 0, parent: MemoryInterface = null, minAlloc: Int = 1) extends AddressSpace(size, base, parent) {
  val allocator = BuddyAllocator(size, minAlloc)

  def allocRegion(size: Int): MemoryRegion = {
    val baseAddr = allocator.malloc(size)
    val memoryRegionInst = new MemoryRegion(size, baseAddr)
    registerRegion(memoryRegionInst, baseAddr, size)
    memoryRegionInst
  }

  def freeRegion(region: MemoryRegion) = {
    unRegisterRegion(region)
    allocator.free(region.base)
  }

}
