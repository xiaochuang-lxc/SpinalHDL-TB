package tb.memory

class WindowPool(size: BigInt, parent: MemoryInterface, base: BigInt = 0, minAlloc: Int = 1) extends Window(size, parent, base) {
  val allocator = BuddyAllocator(size, minAlloc)

  def allocWindow(size: BigInt) = {
    createWindow(allocator.malloc(size), size)
  }

  def freeWindow(window: Window) = {
    allocator.free(window.base - this.base)
  }
}
