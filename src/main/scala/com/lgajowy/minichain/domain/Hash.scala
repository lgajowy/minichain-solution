package com.lgajowy.minichain.domain

import com.lgajowy.minichain.base.BasePrimitives.{ Bytes, Number }

case class Hash(bytes: Bytes) {
  def toNumber(): Number = Number(1, bytes)

  def toHexString(): String = toHexString(bytes)

  private def toHexString(bytes: Bytes): String =
    "0x" + bytes.map(b => String.format("%02X", Byte.box(b))).mkString("")
}
