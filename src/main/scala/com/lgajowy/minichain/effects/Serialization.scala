package com.lgajowy.minichain.effects

import cats.Applicative
import com.lgajowy.minichain.BasePrimitives.Bytes
import com.lgajowy.minichain.domain.{Block, BlockTemplate, Nonce}

trait Serialization[F[_]] {

  def serialize(block: Block): F[Bytes]

  def serialize(blockTemplate: BlockTemplate): F[Bytes]

  def serialize(nonce: Nonce): F[Bytes]
}

object Serialization {
  def apply[F[_]: Serialization]: Serialization[F] = implicitly

  implicit def make[F[_]: Applicative]: Serialization[F] = new Serialization[F] {
    override def serialize(block: Block): F[Bytes] = {
      Applicative[F].pure(
        Array(
          BigInt(block.index.value).toByteArray,
          block.parentHash.bytes,
          block.transactions.flatMap(_.data.getBytes()).toArray,
          block.miningTarget.value.toByteArray,
          serializeNonce(block.nonce)
        ).flatten
      )
    }

    override def serialize(blockTemplate: BlockTemplate): F[Bytes] = Applicative[F].pure(
      Array(
        BigInt(blockTemplate.index.value).toByteArray,
        blockTemplate.parentHash.bytes,
        blockTemplate.transactions.flatMap(_.data.getBytes()).toArray,
        blockTemplate.miningTarget.value.toByteArray
      ).flatten
    )

    override def serialize(nonce: Nonce): F[Bytes] = Applicative[F].pure(serializeNonce(nonce))

    private def serializeNonce(nonce: Nonce): Bytes = BigInt(nonce.value).toByteArray
  }
}
