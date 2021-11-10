package com.lgajowy.minichain.domain

/*
 Map is good enough for starters. To make it even more efficient,
 once could probably use a trie to save blocks, since the hashes are quite long
 and we lots of blocks are expected to be saved in the Blockchain.

 BTW: I think tries are used by GIT when it saves objects.
 */
case class Blockchain(blocks: Vector[Block], hashToBlock: Map[Hash, Block])

object Blockchain {
  def apply(genesisBlock: Block, genesisHash: Hash): Blockchain = {
    Blockchain(Vector(genesisBlock), Map(genesisHash -> genesisBlock))
  }
}
