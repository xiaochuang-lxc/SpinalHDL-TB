package tb.memory

abstract class Region(size: BigInt, base: BigInt = 0, parent: MemoryInterface = null) extends MemoryInterface(size, base, parent) {}
