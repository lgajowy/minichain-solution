package com.lgajowy.minichain.algebras

import cats.Applicative
import com.lgajowy.minichain.domain.{Block, Hash, MiningTarget}

trait BlockVerification[F[_]] {
  def verify(block: Block, blockHash: Hash, miningTarget: MiningTarget): F[Boolean]
}

// TODO change
object BlockVerification {
  def make[F[_]: Applicative](): BlockVerification[F] = new BlockVerification[F] {
    override def verify(block: Block, blockHash: Hash, miningTarget: MiningTarget): F[Boolean] =
      Applicative[F].pure(blockHash.toNumber() < miningTarget.value)

  }
}
