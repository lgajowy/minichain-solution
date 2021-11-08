package com.lgajowy.minichain.programs

import cats.effect.kernel.Async
import cats.implicits._
import com.lgajowy.minichain.BasePrimitives.Bytes
import com.lgajowy.minichain.algebras.{ BlockVerification, HashDigests, Nonces }
import com.lgajowy.minichain.domain.MiningTarget.StdMiningTarget
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.effects.{ Race, Serialization }

final case class Miner[F[_]: Async: Serialization: Race](
  hashDigests: HashDigests[F],
  nonces: Nonces[F],
  blocks: BlockVerification[F],
  parallelism: Int
) {

  def mine(index: Index, parentHash: Hash, transactions: List[Transaction], miningTarget: MiningTarget): F[Block] = {

    val blockTemplateBytes = Serialization[F].serialize(BlockTemplate(index, parentHash, transactions, miningTarget))

    def task(blockTemplateBytes: F[Bytes]): F[Block] = {
      val nonceCandidate: F[(Nonce, Boolean)] = for {
        nonce <- nonces.getNextNonce()
        nonceBytes <- Serialization[F].serialize(nonce)
        blockBytes <- blockTemplateBytes.map(Array(_, nonceBytes).flatten)
        blockHash <- hashDigests.getHashDigest(blockBytes)
        block = Block(index, parentHash, transactions, miningTarget, nonce)
        isVerified <- blocks.verify(block, blockHash)
      } yield (nonce, isVerified)

      nonceCandidate
        .iterateUntil { case (_, isValid) => isValid }
        .map { case (nonce, _) => Block(index, parentHash, transactions, miningTarget, nonce) }
    }

    Race[F].race[Bytes, Block](parallelism, blockTemplateBytes, task)
  }

  def mineGenesis(): F[Block] = {
    for {
      zeroHash <- hashDigests.zeroHash()
      genesis <- mine(
        Index(0),
        zeroHash,
        List(Transaction("Hello Blockchain, this is Genesis :)")),
        StdMiningTarget
      )
    } yield genesis
  }
}
