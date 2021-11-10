package com.lgajowy.minichain.domain

case class Block(
  index: Index,
  parentHash: Hash,
  transactions: List[Transaction],
  miningTarget: MiningTarget,
  nonce: Nonce
)
