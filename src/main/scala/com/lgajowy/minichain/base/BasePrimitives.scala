package com.lgajowy.minichain.base

object BasePrimitives {
  type Number = BigInt
  val Number = BigInt
  type Bytes = Array[Byte]
  val Bytes = new Array[Byte](_: Int)
}
