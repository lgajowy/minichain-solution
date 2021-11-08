package com.lgajowy.minichain.domain


case class Block(
  index: Index,
  parentHash: Hash,
  transactions: Seq[Transaction],
  miningTarget: MiningTarget,
  nonce: Nonce
)

object Block {
  def toBytes(block: Block): Array[Byte] = {
    Array(
      block.index.value.toByteArray,
      block.parentHash.bytes,
      block.transactions.flatMap(_.data.getBytes()).toArray,
      block.miningTarget.value.toByteArray,
      Nonce.toBytes(block.nonce)
    ).flatten
  }
}
