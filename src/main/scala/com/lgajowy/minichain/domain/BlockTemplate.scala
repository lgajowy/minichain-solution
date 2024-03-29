package com.lgajowy.minichain.domain

case class BlockTemplate(
  index: Index,
  parentHash: Hash,
  transactions: List[Transaction],
  miningTarget: MiningTarget
) {
  def toBlock(nonce: Nonce): Block = {
    Block(index, parentHash, transactions, miningTarget, nonce)
  }
}
