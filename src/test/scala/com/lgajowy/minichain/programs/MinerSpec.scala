package com.lgajowy.minichain.programs

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.lgajowy.minichain.algebras.{BlockVerification, HashDigests, Nonces}
import com.lgajowy.minichain.domain.MiningTarget.StdMiningTarget
import com.lgajowy.minichain.domain._
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class MinerSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  private val hashProvider = HashDigests.makeSHA256[IO]()

  private final val parentHash: Hash = hashProvider.zeroHash

  "Miner.mine()" should "mine a block" in {
    val miner = setupMiner(hashProvider)

    val target = MiningTarget.byLeadingZeros(1)

    val transactions = List(Transaction("Simple transaction"))

    // podaj hash provider

    // Sprawdz czy zwraca bloki, dla których weryfikator jest true
    // Sprawdz czy hash jest niższy niż trudność
    //
    // TODO: pass blocktemplate
    // 1. mined block
    //
    val result: IO[Block] = miner.mine(
      BlockTemplate(
        Index(0),
        parentHash,
        transactions,
        target
      )
    )

    result.asserting(block => {
      block.index shouldBe Index(0)
      block.miningTarget shouldBe target
      block.parentHash shouldBe parentHash
      block.transactions shouldBe transactions
      block.nonce should not be null
    })
  }

  "Mining genesis" should "mine a very specific genesis block" in {
    val miner = setupMiner(hashProvider)

    miner
      .mineGenesis()
      .asserting(block => {
        block.index shouldBe Index(0)
        block.miningTarget shouldBe StdMiningTarget
        block.parentHash shouldBe parentHash
        block.transactions shouldBe List(Transaction("Hello Blockchain, this is Genesis :)"))
        block.nonce should not be null
      })
  }

  def setupMiner(hashProvider: HashDigests[IO]): Miner[IO] = {
    val nonceProvider = Nonces.make[IO]()
    val blockVerification = BlockVerification.make[IO](hashProvider)
    Miner[IO](hashProvider, nonceProvider, blockVerification, 1)
  }
}
