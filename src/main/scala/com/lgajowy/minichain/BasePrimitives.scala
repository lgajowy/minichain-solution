package com.lgajowy.minichain

object BasePrimitives {
  type Number = BigInt
  val Number = BigInt
  type Bytes = Array[Byte]
  val Bytes = new Array[Byte](_: Int)
}
