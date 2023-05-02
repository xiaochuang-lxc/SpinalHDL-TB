package tb.memory

import spinal.core.sim._
import spinal.core.{Endianness, LITTLE}
import tb.Utils.{BigInt2ByteArray, ByteArray2BigInt}

abstract class MemoryInterface(val size: BigInt, var base: BigInt = 0, var parent: MemoryInterface = null) {
  def getAbsoluteAddress(address: BigInt): BigInt = {
    checkRange(address)
    address + base
  }

  /**
   * 继承该类的子类需实现该方法
   *
   * @param address 待读取的起始地址
   * @param length  待读取的长度
   * @param kwargs  用户自定义参数列表
   * @return 返回读取的指定地址指定长度的Array[Byte]
   */
  def readPrivate(address: BigInt, length: Int, kwargs: Map[String, BigInt] = null): Array[Byte]

  def readDwords(address: BigInt, count: Int, endianness: Endianness = LITTLE, kwargs: Map[String, BigInt] = null): Array[BigInt] = {
    readWords(address, count, 4, endianness, kwargs)
  }

  def readWords(address: BigInt, count: Int, ws: Int = 2, endianness: Endianness = LITTLE, kwargs: Map[String, BigInt] = null): Array[BigInt] = {
    val rdata = read(address, count * ws, kwargs)
    val rWords = Array[BigInt]().toBuffer
    for (index <- 0 until count) {
      rWords.append(ByteArray2BigInt(rdata.slice(index * ws, (index + 1) * ws), endianness))
    }
    rWords.toArray
  }

  def read(address: BigInt, length: Int, kwargs: Map[String, BigInt] = null): Array[Byte] = {
    checkRange(address, length)
    readPrivate(address, length, kwargs)
  }

  /**
   * check if the address and length is in this memory interface
   *
   * @param address base address
   * @param length  length of region
   */
  def checkRange(address: BigInt, length: BigInt = 0) = {
    if (address < 0 || length > this.size) {
      simFailure(s"address:$address\t length:$length  out of range")
    }
    if (length < 0)
      simFailure(s"$length invalid length")
    if ((address + length) > this.size)
      simFailure(s"address:$address\t length:$length  operation out of range")
  }

  def readQords(address: BigInt, count: Int, endianness: Endianness = LITTLE, kwargs: Map[String, BigInt] = null): Array[BigInt] = {
    readWords(address, count, 8, endianness, kwargs)
  }

  def readByte(address: BigInt, kwargs: Map[String, BigInt] = null): Byte = {
    read(address, 1, kwargs).head
  }

  def readWord(address: BigInt, endianness: Endianness = LITTLE, kwargs: Map[String, BigInt] = null): BigInt = {
    readWords(address, 1, 2, endianness, kwargs).head
  }

  def readDword(address: BigInt, endianness: Endianness = LITTLE, kwargs: Map[String, BigInt] = null): BigInt = {
    readWords(address, 1, 4, endianness, kwargs).head
  }

  def readQword(address: BigInt, endianness: Endianness = LITTLE, kwargs: Map[String, BigInt] = null): BigInt = {
    readWords(address, 1, 8, endianness, kwargs).head
  }

  /**
   * 继承该类的子类需实现该方法
   *
   * @param address 待写入的起始地址
   * @param data    待写入数据
   * @param info    附加信息，特殊场景可使用
   */
  def writePrivate(address: BigInt, data: Array[Byte], kwargs: Map[String, BigInt] = null): Unit

  def writeQwords(address: BigInt, data: Array[BigInt], endianness: Endianness = LITTLE, kwargs: Map[String, BigInt] = null) = {
    writeWords(address, data, 8, endianness, kwargs)
  }

  def writeByte(address: BigInt, data: Byte, kwargs: Map[String, BigInt] = null) = {
    write(address, Array(data), kwargs)
  }

  def writeWord(address: BigInt, data: BigInt, endianness: Endianness = LITTLE, kwargs: Map[String, BigInt] = null) = {
    writeWords(address, Array(data), 2, endianness, kwargs)
  }

  def writeDWord(address: BigInt, data: BigInt, endianness: Endianness = LITTLE, kwargs: Map[String, BigInt] = null) = {
    writeDwords(address, Array(data), endianness, kwargs)
  }

  def writeDwords(address: BigInt, data: Array[BigInt], endianness: Endianness = LITTLE, kwargs: Map[String, BigInt] = null) = {
    writeWords(address, data, 4, endianness, kwargs)
  }

  def writeWords(address: BigInt, data: Array[BigInt], ws: Int = 2, endianness: Endianness = LITTLE, kwargs: Map[String, BigInt] = null) = {
    val writeData = data.map(BigInt2ByteArray(_, ws, endianness)).reduce(_ ++ _)
    write(address, writeData, kwargs)
  }

  def write(address: BigInt, data: Array[Byte], kwargs: Map[String, BigInt] = null) = {
    checkRange(address, data.length)
    writePrivate(address, data, kwargs)
  }

  def writeQword(address: BigInt, data: BigInt, endianness: Endianness = LITTLE, kwargs: Map[String, BigInt] = null) = {
    writeWords(address, Array(data), 8, endianness, kwargs)
  }

  /**
   * 创建指定长度的Window窗口并映射到内存空间
   * 通过Window实现读写指定地址addr的数据等效于通过MemoryInterface读写addr+offset地址
   *
   * @param offset Window在Memory空间中的偏移地址
   * @param size   Window的大小
   * @return Window实例
   */
  def createWindow(offset: BigInt, size: BigInt): Window = {
    checkRange(offset, size)
    new Window(size, this, getAbsoluteAddress(offset))
  }

  /**
   * 创建指定大小的WindowPool并映射到指定内存空间
   * 与Window不同的是WindowPool带有内存分配机制，可实现类似软件的malloc，free机制
   *
   * @param offset Window在Memory空间中的偏移地址
   * @param size   WindowPool的大小
   * @return WindowPool实例
   */
  def createWindowPool(offset: BigInt, size: BigInt): WindowPool = {
    checkRange(offset, size)
    new WindowPool(size, this, getAbsoluteAddress(offset))
  }
}
