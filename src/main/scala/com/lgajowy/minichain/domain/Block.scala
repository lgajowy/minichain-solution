package com.lgajowy.minichain.domain

import com.lgajowy.minichain.domain.Base.{ Nonce, Number }

case class Block(
  index: Int,
  parentHash: Hash,
  transactions: Seq[Transaction],
  miningTargetNumber: Number,
  nonce: Nonce
)
