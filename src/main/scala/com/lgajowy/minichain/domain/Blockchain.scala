package com.lgajowy.minichain.domain

import com.lgajowy.minichain.base.BasePrimitives.Unknown

// A Blockchain is a sequence of blocks, each one having an index.
// The index of a block is the index of its parent plus one.
// A Blockchain always has a genesis block at index 0, which is the lowest index.
sealed trait Blockchain {
  // Add a block to the chain.
  // The return type is up to you, as explained in the definition of Unknown.
  def append(block: Block): Unknown

  // Find a block by index.
  def findByIndex(index: Int): Unknown

  // Find a block by hash.
  def findByHash(hash: Hash): Unknown

  // Find a common ancestor between this blockchain and that blockchain.
  def common_ancestor(that: Blockchain): Unknown
}

// Implement an in-memory blockchain that internally has an indexing data structure.
// The purpose of this internal data structure is to avoid traversing the linked list
// of blocks when answering queries like findByIndex.
class FastBlockchain extends Blockchain {
  def append(block: Block): Unknown = ???

  def findByIndex(index: Int): Unknown = ???

  def findByHash(hash: Hash): Unknown = ???

  def common_ancestor(that: Blockchain): Unknown = ???
}
