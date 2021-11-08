package com.lgajowy.minichain.programs

import cats.effect.{ Concurrent, IO }
import cats.{ Foldable, Monad }
import cats.effect.kernel.Async
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
  def make[F[_]: Async](
    blockVerifier: BlockVerifier[F],
    nonceProvider: NonceProvider[F],
    parallelism: Int
  ): Miner[F] = new Miner[F] {

    override def mine(
      index: Index,
      parentHash: Hash,
      transactions: Seq[Transaction],
      miningTarget: MiningTarget
    ): F[Block] = {

      def task(): F[Block] = {
        val candidateGeneration: F[(Block, Boolean)] = for {
          nonce <- nonceProvider.getNextNonce()
          block = Block(index, parentHash, transactions, miningTarget, nonce)
          verificationResult <- blockVerifier.verify(block)
        } yield (block, verificationResult)

        candidateGeneration
          .iterateUntil { case (_, isVerificationPositive) => isVerificationPositive }
          .map { case (block, _) => block }
      }

      val tasks: List[F[Block]] = (0 until parallelism).map(_ => task()).toList

      Foldable[List]
        .foldLeft(tasks.tail, tasks.head) { (acc, next) =>
          Async[F]
            .race(acc, next)
            .map {
              case Right(value) => value
              case Left(value)  => value
            }
        }
    }
  }
}
