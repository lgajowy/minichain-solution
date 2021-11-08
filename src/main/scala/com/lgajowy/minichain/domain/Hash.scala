package com.lgajowy.minichain.domain

import com.lgajowy.minichain.tools.BasePrimitives.{ Bytes, Number }

case class Hash(bytes: Bytes)

object Hash {
  def toHexString(hash: Hash): String = toHexString(hash.bytes)

  private def toHexString(bytes: Array[Byte]): String =
    "0x" + bytes.map(b => String.format("%02X", Byte.box(b))).mkString("")
}
