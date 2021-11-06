package com.lgajowy.minichain.domain

case class Block(
  index: Index,
  parentHash: Hash,
  transactions: Seq[Transaction],
  miningTargetNumber: MiningTarget,
  nonce: Nonce
)
