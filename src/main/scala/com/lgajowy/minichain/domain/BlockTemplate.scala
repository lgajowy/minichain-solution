package com.lgajowy.minichain.domain

case class BlockTemplate(
  index: Index,
  parentHash: Hash,
  transactions: Seq[Transaction],
  miningTarget: MiningTarget
)
