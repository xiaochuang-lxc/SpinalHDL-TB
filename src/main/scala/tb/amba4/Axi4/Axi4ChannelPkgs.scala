package tb.amba4.Axi4

import tb.Transaction

case class Axi4AxPkg(
                      addr: BigInt,
                      id: BigInt,
                      region: Int,
                      len: Int,
                      size: Int,
                      burst: Int,
                      lock: Int,
                      cache: Int,
                      qos: Int,
                      user: BigInt,
                      prot: Int
                    ) extends Transaction {
  val transferBytesPerCycle=1<<size
  def generatekwargsMap(): Map[String, BigInt] = {
    Map(
      "id" -> id,
      "region" -> region,
      "len" -> len,
      "size" -> size,
      "burst" -> burst,
      "lock" -> lock,
      "cache" -> cache,
      "qos" -> qos,
      "prot" -> prot,
      "aruser" -> user
    )
  }

  def validCheck()={
    //cross 4k check
    val maxBytesAllowed=4096-(addr.toInt&4095)
    val addrOffset=addr.toInt&(transferBytesPerCycle-1)
    val maxLen=(maxBytesAllowed+addrOffset)/transferBytesPerCycle
    val lenAllowed=scala.math.min(maxLen,256)
    assert(len<lenAllowed,s"this cmd may cross 4k:${this.toString}")
  }

  override def toString: String = f"addr:0x$addr%x\tid:$id\tregion:$region\tlen:$len\tsize:$size\tburst:$burst\tlock:$lock\tcache:$cache\tqos:$qos\tprot:$prot\tuser:$user%x"
}

case class Axi4WPkg(
                     data: BigInt,
                     strb: BigInt,
                     user: BigInt,
                     last: Boolean
                   ) extends Transaction {
  def generatekwargsMap(awCmd: Axi4AxPkg): Map[String, BigInt] = {
    Map(
      "id" -> awCmd.id,
      "region" -> awCmd.region,
      "len" -> awCmd.len,
      "size" -> awCmd.size,
      "burst" -> awCmd.burst,
      "lock" -> awCmd.lock,
      "cache" -> awCmd.cache,
      "qos" -> awCmd.qos,
      "prot" -> awCmd.prot,
      "awuser" -> awCmd.user,
      "wuser" -> user
    )
  }
}

case class Axi4BPkg(
                     id: BigInt,
                     resp: Int,
                     user: BigInt
                   ) extends Transaction {
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

case class Axi4RPkg(
                     data: BigInt,
                     id: BigInt,
                     resp: Int,
                     last: Boolean,
                     user: BigInt
                   ) extends Transaction {
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