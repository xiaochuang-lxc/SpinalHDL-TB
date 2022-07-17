package tb.driver

import tb.Transaction

abstract class Driver {

  var transactionCount: Long = 0

  def sendData[T <: Transaction](transaction: T): Unit

  def drive[T <: Transaction](transaction: T) = {
    sendData(transaction)
    transactionCal(transaction)
  }

  def transactionCal[T <: Transaction](transaction: T): Unit = {
    transactionCount += 1
  }
}
