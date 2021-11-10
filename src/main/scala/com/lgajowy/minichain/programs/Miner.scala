package com.lgajowy.minichain.programs

import cats.effect.kernel.Async
import cats.implicits._
import com.lgajowy.minichain.algebras.{ BlockVerifier, HashProvider, NonceProvider }
import com.lgajowy.minichain.base.BasePrimitives.Bytes
import com.lgajowy.minichain.domain.MiningTarget.StdMiningTarget
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.effects.Race
import com.lgajowy.minichain.ext.Serializer.serialize
import com.lgajowy.minichain.ext.Sha256

final case class Miner[F[_]: Async: Race](
  hashProvider: HashProvider[F],
  nonces: NonceProvider[F],
  blocks: BlockVerifier[F],
  parallelism: Int
) {

  def mine(blockTemplate: BlockTemplate): F[Block] = {

    val blockTemplateBytes: Bytes = serialize(blockTemplate)

    def task(blockTemplateBytes: Bytes): F[Block] = {
      val nonceCandidate: F[(Nonce, Boolean)] = for {
        nonce <- nonces.getNextNonce()
        nonceBytes = serialize(nonce)
        blockBytes = blockTemplateBytes ++ nonceBytes
        blockHash <- hashProvider.getHashDigest(blockBytes)
        isVerified <- blocks.verify(blockHash, blockTemplate.miningTarget)
      } yield (nonce, isVerified)

      nonceCandidate
        .iterateUntil { case (_, isValid) => isValid }
        .map { case (nonce, _) => blockTemplate.toBlock(nonce) }
    }

    Race[F].race(parallelism, task(blockTemplateBytes))
  }

  def mineGenesis(): F[Block] = {
    val genesisTemplate = BlockTemplate(
      Index(0),
      Hash(Sha256.ZeroHash),
      List(Transaction("Hello Blockchain, this is Genesis :)")),
      StdMiningTarget
    )

    mine(genesisTemplate)
  }
}
