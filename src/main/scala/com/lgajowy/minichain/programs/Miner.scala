package com.lgajowy.minichain.programs

import cats.effect.kernel.Async
import cats.implicits._
import com.lgajowy.minichain.base.BasePrimitives.Bytes
import com.lgajowy.minichain.algebras.{BlockVerification, HashDigests, Nonces}
import com.lgajowy.minichain.domain.MiningTarget.StdMiningTarget
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.ext.Serializer.serialize
import com.lgajowy.minichain.effects.Race

final case class Miner[F[_]: Async: Race](
  hashDigests: HashDigests[F],
  nonces: Nonces[F],
  blocks: BlockVerification[F],
  parallelism: Int
) {

  def mine(blockTemplate: BlockTemplate): F[Block] = {

    val blockTemplateBytes: Bytes = serialize(blockTemplate)

    def task(blockTemplateBytes: Bytes): F[Block] = {
      val nonceCandidate: F[(Nonce, Boolean)] = for {
        nonce <- nonces.getNextNonce()
        nonceBytes = serialize(nonce)
        // TODO: Check if it works well
        blockBytes = blockTemplateBytes ++ nonceBytes
        blockHash <- hashDigests.getHashDigest(blockBytes)
        block = blockTemplate.toBlock(nonce)
        isVerified <- blocks.verify(block, blockHash, block.miningTarget)
      } yield (nonce, isVerified)

      nonceCandidate
        .iterateUntil { case (_, isValid) => isValid }
        .map { case (nonce, _) => blockTemplate.toBlock(nonce) }
    }

    Race[F].race[Bytes, Block](parallelism, blockTemplateBytes, task)
  }

  def mineGenesis(): F[Block] = {
    val genesisTemplate = BlockTemplate(
      Index(0),
      hashDigests.zeroHash,
      List(Transaction("Hello Blockchain, this is Genesis :)")),
      StdMiningTarget
    )

    mine(genesisTemplate)
  }
}
