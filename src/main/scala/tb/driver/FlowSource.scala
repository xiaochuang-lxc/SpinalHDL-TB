package tb.driver

import spinal.core._
import spinal.core.sim._
import spinal.lib.Flow
import spinal.sim.SimThread
import tb.{Event, Transaction}

import scala.collection.mutable.Queue

abstract class FlowSource[T <: Data](bus: Flow[T], clockDomain: ClockDomain, queueOccupancyLimit: Int = 8) extends BusDriver(bus, clockDomain) {
  val transactionQueue = Queue[Transaction]()
  val active = Event()
  var driveThread: SimThread = null

  override def init(): Unit = {
    bus.valid #= false
    bus.payload.flattenForeach(setBigInt(_, 0))
    active.clear()
    transactionQueue.clear()
    transactionCount = 0
  }

  def stop() = {
    if (driveThread != null)
      driveThread.terminate()
  }

  def send(transaction: Transaction) = {
    waitUntil(transactionQueue.size < queueOccupancyLimit)
    transactionQueue.enqueue(transaction)
  }

  def full = transactionQueue.size == queueOccupancyLimit

  def idle = transactionQueue.isEmpty & !active.fired

  def streamPause(): Boolean

  def start() = {
    driveThread = fork {
      while (true) {
        if (!transactionQueue.isEmpty && !streamPause()) {
          active.set()
          bus.valid #= true
          drive(transactionQueue.dequeue())
          clockDomain.waitSampling()
        } else {
          bus.valid #= false
          active.clear()
          clockDomain.waitSampling()
        }
      }
    }
  }

}
