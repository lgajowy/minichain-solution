package com.lgajowy.minichain.domain

case class Chain(blocks: Vector[Block], hashToBlock: Map[Hash, Block])

object Chain {
  def apply(genesisBlock: Block, genesisHash: Hash): Chain = {
    Chain(Vector(genesisBlock), Map(genesisHash -> genesisBlock))
  }
}
