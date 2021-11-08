package com.lgajowy.minichain

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{Async, IO}
import com.lgajowy.minichain.algebras.{HashDigests, HashTransformer, Nonces}
import com.lgajowy.minichain.domain.MiningTarget.StdMiningTarget
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.programs.Miner
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers


class MinerSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  private final val parentHash: Hash = Hash(Array[Byte]())

  private val stubHashTransformer: HashTransformer[IO] = new HashTransformer[IO] {
    override def toNumber(hash: Hash): IO[BigInt] = IO.pure(0)

    override def toHexString(hash: Hash): IO[String] = IO.pure("")
  }

  def setupMiner[F[_]: Async](hashTransformer: HashTransformer[F]): Miner[F] = {
    val nonceProvider = Nonces.make[F]()
    val hashProvider = HashDigests.makeSHA256[F]()
    Miner[F](hashProvider, hashTransformer, nonceProvider, 1)
  }

  "Miner.mine()" should "mine a block" in {
    val miner = setupMiner[IO](stubHashTransformer)

    val target = MiningTarget.byLeadingZeros(31)

    val transactions = Seq(Transaction("Simple transaction"))

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
    val miner = setupMiner[IO](stubHashTransformer)

    miner
      .mineGenesis(parentHash)
      .asserting(block => {
        block.index shouldBe Index(0)
        block.miningTarget shouldBe StdMiningTarget
        block.parentHash shouldBe parentHash
        block.transactions shouldBe Seq(Transaction("Hello Blockchain, this is Genesis :)"))
        block.nonce should not be (null)
      })
  }
}
