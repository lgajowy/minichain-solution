package com.lgajowy.minichain

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.lgajowy.minichain.algebras.{ BlockVerification, HashDigests, Nonces }
import com.lgajowy.minichain.domain.MiningTarget.StdMiningTarget
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.effects.TransformsHashes
import com.lgajowy.minichain.programs.Miner
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class MinerSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  private final val parentHash: Hash = Hash(Array[Byte]())
//
//  private implicit val stubHashTransformer: TransformsHashes[IO] = new TransformsHashes[IO] {
//    override def toNumber(hash: Hash): IO[Number] = IO.pure(0)
//
//    override def toHexString(hash: Hash): IO[String] = IO.pure("")
//  }

  def setupMiner(): Miner[IO] = {
    val nonceProvider = Nonces.make[IO]()
    val hashProvider = HashDigests.makeSHA256[IO]()
    val blockVerification = BlockVerification.make[IO]()
    Miner[IO](hashProvider, nonceProvider, blockVerification, 1)
  }

  "Miner.mine()" should "mine a block" in {
    val miner = setupMiner()

    val target = MiningTarget.byLeadingZeros(0)

    val transactions = List(Transaction("Simple transaction"))


    // podaj hash provider

    // Sprawdz czy zwraca bloki, dla których weryfikator jest true
    // Sprawdz czy hash jest niższy niż trudność
    //
    // TODO: pass blocktemplate
    // 1. mined block
    //
    val result: IO[Block] = miner.mine(
      Index(0),
      parentHash,
      transactions,
      target
    )

    result.asserting(block => {
      block.index shouldBe Index(0)
      block.miningTarget shouldBe target
      block.parentHash shouldBe parentHash
      block.transactions shouldBe transactions
      block.nonce should not be (null)
    })
  }

  "Mining genesis" should "mine a very specific genesis block" in {
    val miner = setupMiner()

    miner
      .mineGenesis()
      .asserting(block => {
        block.index shouldBe Index(0)
        block.miningTarget shouldBe StdMiningTarget
        block.parentHash shouldBe parentHash
        block.transactions shouldBe List(Transaction("Hello Blockchain, this is Genesis :)"))
        block.nonce should not be (null)
      })
  }
}
