package com.lgajowy.minichain.domain

case class Nonce(value: Long)

object Nonce {
  def toBytes(nonce: Nonce): Array[Byte] = BigInt(nonce.value).toByteArray
}
