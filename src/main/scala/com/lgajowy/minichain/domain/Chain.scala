package com.lgajowy.minichain.domain

// vector blocków - zamienic
// index => hash
// co z tym anyval? newtypes?
//TODO: improve
case class Chain(
  lastBlock: Block,
  indexToHash: Map[Index, Hash],
  hashToBlock: Map[Hash, Block]
)

object Chain {

  def apply(genesisBlock: Block, genesisBlockHash: Hash): Chain = {
    Chain(
      genesisBlock,
      Map(genesisBlock.index -> genesisBlockHash),
      Map(genesisBlockHash -> genesisBlock)
    )
  }
}
