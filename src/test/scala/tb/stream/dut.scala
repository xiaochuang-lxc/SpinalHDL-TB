package tb.stream

import spinal.core._
import spinal.lib._

case class pkgInPort() extends Bundle {
  val data0, data1 = UInt(8 bits)

  def sum = data0 + data1
}

case class dut() extends Component {
  val io = new Bundle {
    val pkgIn = slave(Stream(pkgInPort()))
    val pkgOut = master(Stream(UInt(8 bits)))
  }
  noIoPrefix()
  io.pkgOut <> io.pkgIn.translateWith(io.pkgIn.sum).pipelined(true, true)
}
