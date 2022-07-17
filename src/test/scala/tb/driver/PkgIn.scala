package tb.driver

import tb.Transaction

case class PkgIn(data0: Int, data1: Int) extends Transaction {
  override def toString: String = s"data0=$data0\tdata1=$data1"

  def sum = data0 + data1
}
