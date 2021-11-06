package com.lgajowy.minichain.algebras

import cats.effect.kernel.Sync
import com.lgajowy.minichain.domain.{Block, Hash}
import com.lgajowy.minichain.tools.Sha256

trait HashCalculator[F[_]] {
  def calculate(block: Block): F[Hash]
}

object HashCalculator {
  def make[F[_]: Sync](): HashCalculator[F] = new HashCalculator[F] {
    override def calculate(block: Block): F[Hash] = {
      Sync[F].delay {
        val hash = Sha256(block.toString.getBytes())
        Hash(hash)
      }
    }
  }
}
