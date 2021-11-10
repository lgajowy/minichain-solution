package com.lgajowy.minichain.algebras

import cats.Applicative
import com.lgajowy.minichain.domain.{Hash, MiningTarget}

trait BlockVerifier[F[_]] {
  def verify(blockHash: Hash, miningTarget: MiningTarget): F[Boolean]
}

object BlockVerifier {
  def make[F[_]: Applicative](): BlockVerifier[F] = new BlockVerifier[F] {
    override def verify(blockHash: Hash, miningTarget: MiningTarget): F[Boolean] =
      Applicative[F].pure(blockHash.toNumber() < miningTarget.value)
  }
}
