package tb.amba4.AxiLite4

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._

case class dut() extends Component {
  val io = new Bundle {
    val alite = slave(AxiLite4(10, 32))
  }
  noIoPrefix()
  val aliteTmp = cloneOf(io.alite)
  aliteTmp <> io.alite
  aliteTmp.aw.addr.removeAssignments()
  aliteTmp.aw.addr(9 downto 2) := io.alite.aw.addr(9 downto 2)
  aliteTmp.aw.addr(1 downto 0).clearAll()
  aliteTmp.ar.addr.removeAssignments()
  aliteTmp.ar.addr(9 downto 2) := io.alite.ar.addr(9 downto 2)
  aliteTmp.ar.addr(1 downto 0).clearAll()
  val aliteFactory = new AxiLite4SlaveFactory(aliteTmp)
  for (index <- 0 until 64) {
    aliteFactory.createReadAndWrite(UInt(32 bits), index * 4, 0)
  }
}
