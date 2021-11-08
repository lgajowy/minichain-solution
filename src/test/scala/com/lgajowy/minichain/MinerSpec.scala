package com.lgajowy.minichain

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.lgajowy.minichain.algebras.{ BlockVerifier, HashCalculator, NonceProvider }
import com.lgajowy.minichain.domain.MiningTarget.StdMiningTarget
import com.lgajowy.minichain.domain.{ Block, Hash, Index, MiningTarget, Transaction }
import com.lgajowy.minichain.programs.Miner
import com.lgajowy.minichain.tools.Sha256
import com.lgajowy.minichain.tools.Sha256._
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class MinerSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  "Miner.mine" should "mine a block with a very non-demanding target" in {
    val miner = setupMiner()

    val target = MiningTarget.byLeadingZeros(1)

    val result: IO[Block] = miner.mine(
      Index(0),
      Hash(Sha256.ZeroHash),
      Seq(Transaction("Simple transaction")),
      target
    )

    result.asserting(block => {
      block.index shouldBe Index(0)
      block.miningTargetNumber shouldBe target
      block.parentHash shouldBe Hash(ZeroHash)
    })
  }

  "Mining genesis" should "mine a very specific genesis block" in {
    val miner = setupMiner()

    miner
      .mineGenesis()
      .asserting(block => {
        block.index shouldBe Index(0)
        block.miningTargetNumber shouldBe StdMiningTarget
        block.parentHash shouldBe Hash(ZeroHash)
      })

  }

  def setupMiner(): Miner[IO] = {
    val nonceProvider = NonceProvider.make[IO](new Random())
    val hashCalculator = HashCalculator.make[IO]()
    val blockVerifier = BlockVerifier.make[IO](hashCalculator)
    Miner.make[IO](blockVerifier, nonceProvider, 1)
  }
}
