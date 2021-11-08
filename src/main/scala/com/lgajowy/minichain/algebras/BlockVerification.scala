package com.lgajowy.minichain.algebras

import cats.Functor
import cats.implicits._
import com.lgajowy.minichain.domain.{Block, Hash, MiningTarget}
import com.lgajowy.minichain.effects.TransformsHashes

trait BlockVerification[F[_]] {
  def verify(block: Block, blockHash: Hash, miningTarget: MiningTarget): F[Boolean]
  def verify(blockHash: Hash, miningTarget: MiningTarget): F[Boolean]
}

object BlockVerification {
  def make[F[_]: Functor: TransformsHashes](): BlockVerification[F] = new BlockVerification[F] {
    override def verify(blockHash: Hash, miningTarget: MiningTarget): F[Boolean] =
      TransformsHashes[F].toNumber(blockHash).map(_ < miningTarget.value)

    override def verify(block: Block, blockHash: Hash): F[Boolean] = ???
  }
}
