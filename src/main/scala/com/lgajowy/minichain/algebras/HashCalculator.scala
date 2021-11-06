package com.lgajowy.minichain.algebras

import cats.effect.kernel.Sync
import com.lgajowy.minichain.domain.{ Block, Hash }
import com.lgajowy.minichain.tools.BasePrimitives.Bytes
import com.lgajowy.minichain.tools.Sha256

trait HashCalculator[F[_]] {
  def calculate(block: Block): F[Hash]
}

object HashCalculator {
  def make[F[_]: Sync](): HashCalculator[F] = new HashCalculator[F] {
    override def calculate(block: Block): F[Hash] = {
      Sync[F].delay {
        val hash: Bytes = Sha256(
          Array(block.index.value.toByte),
          block.parentHash.bytes,
          block.transactions.flatMap(tx => tx.data.getBytes).toArray,
          Array(block.nonce.value.toByte),
          Array(block.miningTargetNumber.value.toByte)
        )
        Hash(hash)
      }
    }
  }
}
