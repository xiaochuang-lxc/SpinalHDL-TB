package tb.driver

import spinal.core._
import spinal.lib._

case class dataPort() extends Bundle {
  val data0, data1 = UInt(8 bits)

  def sum() = data0 + data1
}

case class dut() extends Component {
  val io = new Bundle {
    val pkgIn = slave(Flow(dataPort()))
    val pktOut = master(Flow(UInt(8 bits)))
  }
  noIoPrefix()
  io.pktOut <> io.pkgIn.translateWith(io.pkgIn.sum()).m2sPipe
}
