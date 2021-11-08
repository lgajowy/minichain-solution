package com.lgajowy.minichain.programs

import cats.Foldable
import cats.effect.kernel.Async
import cats.implicits._
import com.lgajowy.minichain.algebras.{HashProvider, HashTransformer, NonceProvider}
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
    hashProvider: HashProvider[F],
    hashTransformer: HashTransformer[F],
    nonceProvider: NonceProvider[F],
    parallelism: Int
  ): Miner[F] = new Miner[F] {

    override def mine(
      index: Index,
      parentHash: Hash,
      transactions: Seq[Transaction],
      miningTarget: MiningTarget
    ): F[Block] = {

      val blockTemplateBytes: Array[Byte] = BlockTemplate
        .toBytes(BlockTemplate(index, parentHash, transactions, miningTarget))

      def task(): F[Block] = {
        val nonceCandidate: F[(Nonce, Boolean)] = for {
          nonce <- nonceProvider.getNextNonce()
          nonceBytes = Nonce.toBytes(nonce)
          blockBytes = Array(blockTemplateBytes, nonceBytes).flatten
          blockHash <- hashProvider.getHashDigest(blockBytes)
          hashNumber <- hashTransformer.toNumber(blockHash)
          isValid = hashNumber < miningTarget.value

        } yield (nonce, isValid)

        nonceCandidate
          .iterateUntil { case (_, isValid) => isValid }
          .map { case (nonce, _) => Block(index, parentHash, transactions, miningTarget, nonce) }
      }

      inParallel(parallelism, task _)
    }
  }

  private def inParallel[F[_]: Async](parallelism: Int, task: () => F[Block]): F[Block] = {
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
