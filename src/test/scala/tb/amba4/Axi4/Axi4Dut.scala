package tb.amba4.Axi4

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

case class Axi4Dut(config: Axi4Config) extends Component {
  val io = new Bundle {
    val input = slave(Axi4(config))
    val output = master(Axi4(config))

  }
  Axi4SpecRenamer(io.input)
  Axi4SpecRenamer(io.output)
  io.output.aw <-/< io.input.aw
  io.output.w <-/< io.input.w
  io.output.ar <-/< io.input.ar
  io.input.b <-/< io.output.b
  io.input.r <-/< io.output.r
}
