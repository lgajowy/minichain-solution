package com.lgajowy.minichain.domain

object Base {
  type Number = BigInt
  val Number = BigInt
  type Nonce = Long
  type Bytes = Array[Byte]
  val Bytes = new Array[Byte](_: Int)
}
