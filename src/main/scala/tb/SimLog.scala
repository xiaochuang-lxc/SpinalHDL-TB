package tb

import spinal.core.sim._
import tb.SimLog.logLevel

object SimLogLevel extends Enumeration {
  var DEBUG, INFO, WARNING, ERROR = Value
}

class SimLog(name: String = "") {

  private def printMessage(infoLevel: SimLogLevel.Value, s: String) = {
    if (infoLevel >= logLevel) {
      println(s)
    }
  }

  def debug(s: String) = {
    printMessage(SimLogLevel.DEBUG, s"[SIM_DEBUG]  (time:${simTime()}  Name:$name)  $s")
  }

  def info(s: String) = {
    printMessage(SimLogLevel.INFO, s"[SIM_INFO]  (time:${simTime()}  Name:$name)  $s")
  }

  def warning(s: String) = {
    printMessage(SimLogLevel.WARNING, s"[SIM_WARNING]  (time:${simTime()}  Name:$name)  $s")
  }

  def error(s: String) = {
    printMessage(SimLogLevel.ERROR, s"[SIM_ERROR]  (time:${simTime()}  Name:$name)  $s")
    simFailure()
  }

}

object SimLog {
  var logLevel = SimLogLevel.INFO

  def apply(name: String): SimLog = new SimLog(name)

  def setLogLevel(value: SimLogLevel.Value) = {
    logLevel = value
  }
}