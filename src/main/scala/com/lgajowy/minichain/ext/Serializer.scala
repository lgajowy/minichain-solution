package com.lgajowy.minichain.ext

import com.lgajowy.minichain.base.BasePrimitives.{ Bytes, Number }
import com.lgajowy.minichain.domain.{ Block, BlockTemplate, Nonce }

object Serializer {

  def serialize(block: Block): Bytes = {
    Array(
      Number(block.index.value).toByteArray,
      block.parentHash.bytes,
      block.transactions.flatMap(_.data.getBytes()).toArray,
      block.miningTarget.value.toByteArray,
      serialize(block.nonce)
    ).flatten
  }

  def serialize(blockTemplate: BlockTemplate): Bytes =
    Array(
      Number(blockTemplate.index.value).toByteArray,
      blockTemplate.parentHash.bytes,
      blockTemplate.transactions.flatMap(_.data.getBytes()).toArray,
      blockTemplate.miningTarget.value.toByteArray
    ).flatten

  def serialize(nonce: Nonce): Bytes = Number(nonce.value).toByteArray
}
