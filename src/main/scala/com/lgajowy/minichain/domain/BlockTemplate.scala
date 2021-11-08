package com.lgajowy.minichain.domain

case class BlockTemplate(
  index: Index, // to type alias
  parentHash: Hash,
  transactions: Seq[Transaction],
  miningTarget: MiningTarget
)
