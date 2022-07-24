package tb.amba4.AxiLite4

import spinal.core._
import spinal.lib.tools.BigIntToListBoolean
import tb.Transaction
import tb.Utils.BigInt2ByteArray

case class AxiLite4WPkg(data: BigInt, strb: BigInt) extends Transaction {

  def getWriteCmd(addr: BigInt, bytePerBeats: Int): Array[Byte] = {
    val writeDataTmp = BigInt2ByteArray(data, bytePerBeats)
    val strbList = BigIntToListBoolean(strb, bytePerBeats bits)
    (writeDataTmp, strbList).zipped.toArray.filter(_._2).map(_._1)
  }

}
