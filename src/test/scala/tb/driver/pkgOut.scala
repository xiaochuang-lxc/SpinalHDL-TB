package tb.driver

import tb.Transaction

case class pkgOut(sum: Int) extends Transaction {
  override def toString: String = s"sum=${sum}"
}
