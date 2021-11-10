package com.lgajowy.minichain.domain

case class Block(
  index: Int,
  parentHash: Hash,
  transactions: Seq[Transaction],
  miningTarget: MiningTarget,
  nonce: Nonce
) {

  // To get the crypto hash of the block, just feed all fields to SHA-256.
  def cryptoHash: Hash = ???

}
