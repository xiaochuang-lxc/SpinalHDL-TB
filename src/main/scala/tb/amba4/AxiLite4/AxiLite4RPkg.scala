package tb.amba4.AxiLite4

import tb.Transaction

case class AxiLite4RPkg(data: BigInt, resp: Int) extends Transaction {
  def isOkay = resp == 0

  def showResp(): String = {
    resp match {
      case 0 => "OKAY"
      case 1 => "EXOKAY"
      case 2 => "SLVERR"
      case 3 => "DECRR"
    }
  }
}
