package tb.monitor

import spinal.core.sim._
import spinal.sim.SimThread
import tb.{Event, Transaction}

import scala.collection.mutable.{ArrayBuffer, Queue}

abstract class Monitor(callback: (Transaction) => Unit, event: Event = null) {

  val callbackArray = ArrayBuffer[(Transaction) => Unit]()
  val recvQ = Queue[Transaction]()
  var monitorThread: SimThread = null
  var transactionCount: Long = 0
  //if (callback != null)
  //  callbackArray += callback

  /**
   * Sub-classes should override this method to implement the actual receive
   * routine and call :meth:`recv` with the recovered transaction.
   */
  def monitorRecv(): Unit

  def addCallback(callback: Transaction => Unit): Unit = {
    callbackArray += callback
  }

  def recv[T <: Transaction](transaction: T): Unit = {
    callbackArray.foreach(_ (transaction))
    if (callbackArray.isEmpty)
      recvQ.enqueue(transaction)
    if (event != null)
      event.set(transaction)
    transactionCal(transaction)
  }

  def getItem[T <: Transaction]: T = {
    waitUntil(!recvQ.isEmpty)
    recvQ.dequeue().asInstanceOf[T]
  }

  def transactionCal[T <: Transaction](transaction: T): Unit = {
    transactionCount += 1
  }

  def start() = {
    monitorThread = fork(monitorRecv())
  }
}
