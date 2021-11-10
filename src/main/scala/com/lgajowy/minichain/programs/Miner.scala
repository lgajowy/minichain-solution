package com.lgajowy.minichain.programs

import com.lgajowy.minichain.base.BasePrimitives.Bytes
import com.lgajowy.minichain.domain.{ Block, Hash, Transaction }
import com.lgajowy.minichain.ext.Sha256

object Miner {
  // NOTE: A Hash is also a Number, we use the two interchangeably.
  //
  // Mining is about computing hashes until we get something that is less
  // than a given target number.
  // This target serves, in a way, as the maximum possible number that a
  // proof of work computation should produce.
  final val StdMiningTargetNumber = targetByLeadingZeros(1)

  // Whoa! We actually mine the Genesis block.
  // Normally, this is done by the system during bootstrapping
  // and every other block is mined by a miner.
  final val Genesis = Miner.mineNextBlock(
    index = 0, // The very first block
    parentHash = Sha256.ZeroHash, // Let's assume this is by definition for the Genesis block.
    transactions = Seq(Transaction("Hello Blockchain, this is Genesis :)")),
    StdMiningTargetNumber
  )

  // We basically create a target number with the requirement of having
  // some leading zeros. More leading zeros means smaller target number.
  //
  // NOTE: To actually solve the current coding challenge, would you choose a
  // small or a big number of leading zeros?
  def targetByLeadingZeros(zeros: Int): BigInt = {
    require(zeros < Sha256.NumberOfBytes)

    val bytes: Bytes =
      Array.tabulate[Byte](32) { n =>
        if (n < zeros) {
          0
        } else {
          0xff.toByte
        }
      }

    BigInt(1, bytes)
  }

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
    miningTargetNumber: BigInt
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
