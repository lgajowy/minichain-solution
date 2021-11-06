package com.lgajowy.minichain

import com.lgajowy.minichain.domain.Hash.toHexString
import com.lgajowy.minichain.domain.{Block, Hash, Transaction}

trait Miner {
  def mine(index: Int, parentHash: Hash, transactions: Seq[Transaction]): Block
}
