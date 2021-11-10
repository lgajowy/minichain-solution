package minichain.programs

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.lgajowy.minichain.algebras.{ BlockVerifier, HashProvider, NonceProvider }
import com.lgajowy.minichain.base.BasePrimitives.Bytes
import com.lgajowy.minichain.domain.MiningTarget.StdMiningTarget
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.ext.Sha256
import com.lgajowy.minichain.programs.Miner
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class MinerSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  private final val ValidBlockHash = Hash(
    Array[Byte](0, -11, 24, 60, -68, 48, 109, 61, 72, -10, 23, 115, -23, -128, 56, 67, -34, 55, 120, 80, 17, -53, 119,
      113, -45, -107, 46, -92, 12, 120, -99, -70)
  )

  private final val InvalidBlockHash = Hash(
    Array[Byte](-38, 40, -54, -116, 45, 61, 79, -113, -88, 58, 92, -106, -72, 101, 12, 49, 83, -5, 69, -24, -12, -94,
      -81, 42, -25, -50, 18, -95, 84, -108, -105, 108)
  )

  private def stubHashDigests(): HashProvider[IO] = new HashProvider[IO] {
    val stack: mutable.Stack[Hash] = mutable.Stack[Hash]()
    stack.push(ValidBlockHash)
    stack.push(InvalidBlockHash)

    override def getHashDigest(bytes: Bytes): IO[Hash] = IO.pure(stack.pop())
  }

  it should "mine a block " in {
    val miner = setupMiner(stubHashDigests())

    val testBlockTemplate = BlockTemplate(
      Index(100),
      Hash(Bytes(32)),
      List(Transaction("Simple transaction")),
      MiningTarget.StdMiningTarget
    )

    val result: IO[Block] = miner.mine(testBlockTemplate)

    result.asserting(block => {
      block.index shouldBe testBlockTemplate.index
      block.miningTarget shouldBe testBlockTemplate.miningTarget
      block.parentHash shouldBe testBlockTemplate.parentHash
      block.transactions shouldBe testBlockTemplate.transactions
      block.nonce should not be null
    })
  }

  it should "mine a genesis block" in {
    val miner = setupMiner(stubHashDigests())

    miner
      .mineGenesis()
      .asserting(block => {
        block.index shouldBe Index(0)
        block.miningTarget shouldBe StdMiningTarget
        block.parentHash shouldBe Hash(Sha256.ZeroHash)
        block.transactions shouldBe List(Transaction("Hello Blockchain, this is Genesis :)"))
        block.nonce should not be null
      })
  }

  def setupMiner(hashProvider: HashProvider[IO]): Miner[IO] = {
    Miner[IO](hashProvider, NonceProvider.make[IO](), BlockVerifier.make[IO](), 1)
  }
}
