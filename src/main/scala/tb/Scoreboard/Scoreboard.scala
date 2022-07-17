package tb.Scoreboard

import spinal.core.sim._
import tb.monitor.Monitor
import tb.{SimLog, Transaction}

import scala.collection.mutable.ArrayBuffer
import scala.math.min

class Scoreboard(failImmediately: Boolean = true, logName: String = "Scoreboard") {
  val log = new SimLog(logName)

  //val expected=Map[Monitor,ArrayBuffer[Transaction]]()
  def addInterfaces[T <: Transaction](monitor: Monitor, expectedQ: ArrayBuffer[T], reorderDepth: Int = 0, compareFunc: (Transaction) => Unit): Unit = {
    def checkRecv(transaction: Transaction) = {
      val recvTransaction = transaction.asInstanceOf[T]
      var indexMatch = 0
      var matched = false
      for (index <- 0 until min(expectedQ.size, reorderDepth + 1) if !matched) {
        if (expectedQ(index) == recvTransaction) {
          indexMatch == index
          matched = true
        }
      }
      val expectedTransaction = expectedQ.remove(indexMatch)
      if (expectedTransaction != recvTransaction) {
        val expectedTransactionString = expectedTransaction.toString
        val transactionString = recvTransaction.toString
        simFailure(s"${monitor.getClass.getName} Mismatch Error:\n\t" +
          s"transaction:\n\t\t${transactionString}\n\t" +
          s"expected:\n\t\t$expectedTransactionString\n")
      }

    }

    if (compareFunc != null) {
      monitor.addCallback(compareFunc)
    } else {
      monitor.addCallback(checkRecv)
      //expected+=(monitor->expectedQ)
    }

  }
}
