package com.lgajowy.minichain.domain

import com.lgajowy.minichain.tools.BasePrimitives.{Bytes, Number}
import com.lgajowy.minichain.tools.Sha256

case class MiningTarget(value: Number)

object MiningTarget {
  def byLeadingZeros(zeros: Int): MiningTarget = {
    require(zeros < Sha256.NumberOfBytes)

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
