package com.lgajowy.minichain.algebras

import cats.Applicative
import com.lgajowy.minichain.domain.{ Hash, MiningTarget }

trait BlockVerification[F[_]] {
  def verify(blockHash: Hash, miningTarget: MiningTarget): F[Boolean]
}

object BlockVerification {
  def make[F[_]: Applicative](): BlockVerification[F] = new BlockVerification[F] {
    override def verify(blockHash: Hash, miningTarget: MiningTarget): F[Boolean] =
      Applicative[F].pure(blockHash.toNumber() < miningTarget.value)
  }
}
