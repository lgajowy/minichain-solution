package com.lgajowy.minichain.algebras

import cats.Monad
import cats.implicits._
import com.lgajowy.minichain.base.BasePrimitives.Bytes
import com.lgajowy.minichain.domain.{Hash, MiningTarget}

trait BlockVerification[F[_]] {
  def verify(minedBlock: Bytes, blockHash: Hash, miningTarget: MiningTarget): F[Boolean]
}

object BlockVerification {
  def make[F[_]: Monad](hashDigests: HashDigests[F]): BlockVerification[F] = new BlockVerification[F] {
    override def verify(blockBytes: Bytes, blockHash: Hash, miningTarget: MiningTarget): F[Boolean] = {
      for {
        ourHash <- hashDigests.getHashDigest(blockBytes)
        isHashSame = ourHash.toNumber() == blockHash.toNumber()
        hashNumber = blockHash.toNumber()
      } yield isHashSame && (hashNumber < miningTarget.value)
    }
  }
}
