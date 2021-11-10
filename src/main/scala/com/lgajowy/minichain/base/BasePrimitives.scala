package com.lgajowy.minichain.base

object BasePrimitives {
  type Unknown = Nothing

  type Bytes = Array[Byte]
  val Bytes = new Array[Byte](_: Int)

  type Number = BigInt
  val Number = BigInt

}
