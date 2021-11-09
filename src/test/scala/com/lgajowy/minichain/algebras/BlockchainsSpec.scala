package com.lgajowy.minichain.algebras

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.lgajowy.minichain.base.BasePrimitives.Bytes
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.ext.Serializer.serialize
import org.scalatest.EitherValues
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class BlockchainsSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers with EitherValues {

  private val hashDigests: HashDigests[IO] = HashDigests.makeSHA256[IO]()

  private val genesis: Block = Block(
    Index(0),
    hashDigests.zeroHash,
    List(Transaction("")),
    MiningTarget.StdMiningTarget,
    Nonce(123L)
  )

  "Blockchains" should "append a correct block" in {
    val blockchains: Blockchains[IO] = setupBlockchainsInterpreter(
      hashDigests,
      stubBlockVerification(true)
    )

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      chain = Chain(genesis, genesisHash)
      appendedBlock = createBlock(genesisHash)
      appendResult <- blockchains.append(chain, appendedBlock)
    } yield (appendResult, appendedBlock))
      .asserting {
        case (result: Either[Error, Chain], appendedBlock: Block) => {
          result.value.blocks shouldBe Vector(genesis, appendedBlock)
          result.value.hashToBlock.values should contain(appendedBlock)
        }
      }
  }

  "Blockchains" should "not append a block when it wasn't verified successfully" in {
    val blockchains: Blockchains[IO] = setupBlockchainsInterpreter(
      hashDigests,
      stubBlockVerification(false)
    )

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      chain = Chain(genesis, genesisHash)
      blockToAppend = createBlock(genesisHash)
      appendedBlock = blockToAppend
      appendResult <- blockchains.append(chain, appendedBlock)
    } yield appendResult)
      .asserting { result =>
        result shouldBe Left(BlockNotVerifiedProperly())
      }
  }

  "Blockchains" should "not append a block when the parent hash is wrong" in {
    val blockchains: Blockchains[IO] = setupBlockchainsInterpreter(
      hashDigests,
      stubBlockVerification(true)
    )

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      otherBlockHash <- hashDigests.getHashDigest(Bytes(32))
      otherBlock = createBlock(otherBlockHash)
      chain = Chain(
        Vector(genesis, otherBlock),
        Map(genesisHash -> genesis, otherBlockHash -> otherBlock)
      )
      blockToAppend = createBlock(genesisHash)
      appendedBlock = blockToAppend
      appendResult <- blockchains.append(chain, appendedBlock)
    } yield appendResult)
      .asserting { result =>
        result shouldBe Left(IncorrectParentHash())
      }
  }

  private def createBlock(genesisHash: Hash) = {
    Block(
      Index(1),
      genesisHash,
      List(Transaction("Some transaction")),
      MiningTarget.StdMiningTarget,
      Nonce(123L)
    )
  }

  private def stubBlockVerification(result: Boolean): BlockVerification[IO] = new BlockVerification[IO] {
    override def verify(minedBlock: Bytes, blockHash: Hash, miningTarget: MiningTarget): IO[Boolean] = IO.pure(result)
  }

  private def setupBlockchainsInterpreter(
    hashDigests: HashDigests[IO],
    blockVerification: BlockVerification[IO]
  ): Blockchains[IO] = Blockchains.make[IO](blockVerification, hashDigests)
}
