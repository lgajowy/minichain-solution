package com.lgajowy.minichain.domain

sealed trait Blockchain {
  def append(block: Block): Blockchain

  def findByIndex(index: Int): Option[Block]

  def findByHash(hash: Hash): Option[Block]

  def common_ancestor(that: Blockchain): Option[Block]
}

class FastBlockchain extends Blockchain {
  def append(block: Block): Blockchain = ???

  def findByIndex(index: Int): Option[Block] = ???

  def findByHash(hash: Hash): Option[Block] = ???

  def common_ancestor(that: Blockchain): Option[Block] = ???
}
