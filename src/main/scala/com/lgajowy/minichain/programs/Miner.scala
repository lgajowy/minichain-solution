package com.lgajowy.minichain.programs

import cats.effect.Sync
import cats.implicits._
import com.lgajowy.minichain.algebras.{ BlockVerifier, NonceProvider }
import com.lgajowy.minichain.domain.MiningTarget.StdMiningTarget
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.tools.Sha256

trait Miner[F[_]] {

  def mine(index: Index, parentHash: Hash, transactions: Seq[Transaction], miningTarget: MiningTarget): F[Block]

  def mineGenesis(): F[Block] = mine(
    Index(0),
    Hash(Sha256.ZeroHash),
    Seq(Transaction("Hello Blockchain, this is Genesis :)")),
    StdMiningTarget
  )
}

object Miner {
  def make[F[_]: Sync](
    blockVerifier: BlockVerifier[F],
    nonceProvider: NonceProvider[F]
  ): Miner[F] = new Miner[F] {

    override def mine(
      index: Index,
      parentHash: Hash,
      transactions: Seq[Transaction],
      miningTarget: MiningTarget
    ): F[Block] = {

      def task(): F[Block] = {
        for {
          nonce <- nonceProvider.getNextNonce()
          candidate = Block(index, parentHash, transactions, miningTarget, nonce)
          isCorrect <- blockVerifier.verify(candidate)

          minedBlock <- if (isCorrect) {
            Sync[F].delay(candidate)
          } else {
            mine(index, parentHash, transactions, miningTarget)
          }
        } yield minedBlock
      }
      task()
    }
  }
}
