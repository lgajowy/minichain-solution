package com.lgajowy.minichain.domain

import com.lgajowy.minichain.domain.Hash.toNumber

case class BlockTemplate(
  index: Index,
  parentHash: Hash,
  transactions: Seq[Transaction],
  miningTarget: MiningTarget
)

object BlockTemplate {
  def toBytes(block: BlockTemplate): Array[Byte] = {
    Array(
      block.index.value.toByteArray,
      toNumber(block.parentHash).toByteArray,
      block.transactions.flatMap(_.data.getBytes()).toArray,
      block.miningTarget.value.toByteArray
    ).flatten
  }
}
