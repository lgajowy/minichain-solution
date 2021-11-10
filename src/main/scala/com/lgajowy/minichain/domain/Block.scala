package com.lgajowy.minichain.domain

import com.lgajowy.minichain.base.BasePrimitives.{ Nonce, Number }

// Now we are ready to describe the Block.
// Every block has an index, starting from zero (0).
// The block at index 0 is called the Genesis block.
// A block links back to the previous (parent) block.
// Of course, we also record the transactions that this block introduces to our mini-chain.
// We'll see the meaning of the other fields as we move along.
case class Block(
  index: Int,
  parentHash: Hash,
  transactions: Seq[Transaction],
  miningTargetNumber: Number,
  nonce: Nonce
) {

  // To get the crypto hash of the block, just feed all fields to SHA-256.
  def cryptoHash: Hash = ???

}
