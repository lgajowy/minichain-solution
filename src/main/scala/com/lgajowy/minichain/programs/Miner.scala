package com.lgajowy.minichain.programs

import com.lgajowy.minichain.domain.{ Block, Hash, MiningTarget, Transaction }
import com.lgajowy.minichain.ext.Sha256

object Miner {

  // Whoa! We actually mine the Genesis block.
  // Normally, this is done by the system during bootstrapping
  // and every other block is mined by a miner.
  final val Genesis = Miner.mineNextBlock(
    index = 0, // The very first block
    parentHash = Sha256.ZeroHash, // Let's assume this is by definition for the Genesis block.
    transactions = Seq(Transaction("Hello Blockchain, this is Genesis :)")),
    MiningTarget.StdMiningTarget
  )

  // And now let's implement the actual "proof-of-work"-style computation.
  // Compare the parameters of this method with the fields of a Block and
  // you'll see that the only thing missing here is the nonce. Here is why.
  //
  // Initially we have all the fixed elements a block:
  //
  //  - index,
  //  - parentHash,
  //  - transactions,
  //  - miningTargetNumber
  //
  // and by varying the nonce we try to have a block hash that is below the
  // given miningTargetNumber.
  //
  // NOTE Remember that the block hash can be transformed to an actual number,
  //      so we can talk about hash and number interchangeably.
  def mineNextBlock(
    index: Int,
    parentHash: Hash,
    transactions: Seq[Transaction],
    miningTargetNumber: MiningTarget
  ): Block = {
    // Solve this informal inequality for nonce:
    //
    //   Hash(block; nonce).toNumber < miningTargetNumber
    //
    // where Hash(block; nonce) is a function of nonce only, all the other block
    // field values are just the given method arguments.
    ???
  }
}
