package com.lgajowy.minichain.programs

import cats.Foldable
import cats.effect.kernel.Async
import cats.implicits._
import com.lgajowy.minichain.BasePrimitives.Bytes
import com.lgajowy.minichain.algebras.{ HashProvider, HashTransformer, Nonces }
import com.lgajowy.minichain.domain.MiningTarget.StdMiningTarget
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.effects.Serialization

final case class Miner[F[_]: Async: Serialization](
  hashProvider: HashProvider[F],
  hashTransformer: HashTransformer[F],
  nonceProvider: Nonces[F],
  parallelism: Int
) {

  def mine(index: Index, parentHash: Hash, transactions: Seq[Transaction], miningTarget: MiningTarget): F[Block] = {

    val blockTemplateBytes = Serialization[F].serialize(BlockTemplate(index, parentHash, transactions, miningTarget))

    def task(blockTemplateBytes: F[Bytes]): F[Block] = {
      val nonceCandidate: F[(Nonce, Boolean)] = for {
        nonce <- nonceProvider.getNextNonce()
        nonceBytes <- Serialization[F].serialize(nonce)
        blockBytes <- blockTemplateBytes.map(Array(_, nonceBytes).flatten)
        blockHash <- hashProvider.getHashDigest(blockBytes)
        hashAsNumber <- hashTransformer.toNumber(blockHash)
        isValid = hashAsNumber < miningTarget.value

      } yield (nonce, isValid)

      nonceCandidate
        .iterateUntil { case (_, isValid) => isValid }
        .map { case (nonce, _) => Block(index, parentHash, transactions, miningTarget, nonce) }
    }

    inParallel(blockTemplateBytes, task)
  }

  private def inParallel(blockTemplateBytes: F[Bytes], task: F[Bytes] => F[Block]): F[Block] = {
    val tasks: List[F[Block]] = (0 until parallelism).map(_ => task(blockTemplateBytes)).toList
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

  def mineGenesis(parentHash: Hash): F[Block] = mine(
    Index(0),
    parentHash,
    Seq(Transaction("Hello Blockchain, this is Genesis :)")),
    StdMiningTarget
  )
}
