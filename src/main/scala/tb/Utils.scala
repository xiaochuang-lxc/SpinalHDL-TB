package tb

import spinal.core.sim._
import spinal.core.{ClockDomain, Endianness, LITTLE}
import spinal.sim.{SimError, SimThread}

object Utils {
  /**
   * wait all sim thread done
   *
   * @param threads
   */
  def Join(threads: SimThread*) = {
    threads.foreach(_.join())
  }

  /**
   * exit when any of thread isDone
   *
   * @param thread the firist done in threads
   * @return
   */
  def JoinAny(threads: SimThread*): SimThread = {
    waitUntil(threads.map(_.isDone).reduce(_ | _))
    threads.filter(_.isDone).head
  }

  def ByteArray2BigInt(data: Array[Byte], endianness: Endianness = LITTLE): BigInt = {
    val buffer = if (endianness == LITTLE) data.reverse.toBuffer else data.toBuffer
    buffer.prepend(0.toByte)
    BigInt(buffer.toArray)
  }

  def BigInt2ByteArray(data: BigInt, len: Int, endianness: Endianness = LITTLE): Array[Byte] = {
    val dataArray = if (endianness == LITTLE) data.toByteArray.reverse else data.toByteArray
    if (len <= dataArray.length)
      return dataArray.take(len)
    else
      return dataArray ++ Array.fill[Byte](len - dataArray.length)(0.toByte)
  }

  def BooleanList2BigInt(booleanList: Array[Boolean]): BigInt = {
    var result: BigInt = 0
    booleanList.zipWithIndex.filter(_._1).foreach(kv => result = result | (BigInt(1) << kv._2))
    result
  }

  /**
   * Exec Simulation "Body" with timeout
   *
   * @param clockdomain target clockdomain
   * @param cycles      timeout cycles
   * @param body        Simulation body
   */
  def simWithTimeout(clockdomain: ClockDomain, cycles: Int)(body: => Unit): Unit = {
    val startTime = simTime()
    val simThread = fork(body)
    val timeThread = fork {
      clockdomain.waitSampling(cycles)
    }
    val threadFinished = JoinAny(simThread, timeThread)
    if (threadFinished != simThread) {
      simThread.terminate()
      SimError(s"Simulation Timeout: Start Time:$startTime\t End Time:${simTime()}")
    } else
      timeThread.terminate()
  }
}