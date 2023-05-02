package tb

import spinal.core.sim._

case class Event(name: String = "") {
  var data: Any = null
  var fired: Boolean = false

  def set(data: Any): Unit = {
    this.data = data
    fired = true
  }

  def trigger(): Unit = {
    waitUntil(fired)
  }

  def clear(): Unit = {
    fired = false
  }

  def getData[T <: Any](): T = data.asInstanceOf[T]

}
