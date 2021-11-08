package com.lgajowy.minichain.domain

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
      block.parentHash.bytes,
      block.transactions.flatMap(_.data.getBytes()).toArray,
      block.miningTarget.value.toByteArray
    ).flatten
  }
}
