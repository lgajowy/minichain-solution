package com.lgajowy.minichain.domain

import com.lgajowy.minichain.BasePrimitives.{Bytes, Number}

case class MiningTarget(value: Number)

object MiningTarget {

  private final val maxNumberOfBytes = 32

  def byLeadingZeros(zeros: Int): MiningTarget = {
    require(zeros < maxNumberOfBytes)

    val bytes: Bytes =
      Array.tabulate[Byte](32) { n =>
        if (n < zeros) {
          0
        } else {
          0xff.toByte
        }
      }

    MiningTarget(BigInt(1, bytes))
  }

  final val StdMiningTarget = MiningTarget.byLeadingZeros(1)
}
