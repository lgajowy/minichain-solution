package com.lgajowy.minichain.domain

import com.lgajowy.minichain.base.BasePrimitives
import com.lgajowy.minichain.base.BasePrimitives.{Bytes, Number}


// The idea behind any cryptographic hash representation in "mini-chain"
// is to treat it as an immutable array of bytes that can be also viewed
// as a number or a hex string. You will see that the number representation
// is used in the mining process. The hex representation is for logging
// purposes.
case class Hash(bytes: Bytes) {
  def toNumber: Number = Number(1, bytes)

  def toHexString: String = BasePrimitives.toHexString(bytes)
}
