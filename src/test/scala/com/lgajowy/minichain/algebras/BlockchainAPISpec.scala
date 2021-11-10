package com.lgajowy.minichain.algebras

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.lgajowy.minichain.base.BasePrimitives.Bytes
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.ext.Serializer.serialize
import com.lgajowy.minichain.ext.Sha256
import org.scalatest.EitherValues
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class BlockchainAPISpec extends AsyncFlatSpec with AsyncIOSpec with Matchers with EitherValues {

  private val hashDigests: HashProvider[IO] = HashProvider.makeSHA256[IO]()

  private val genesis: Block = Block(
    Index(0),
    Hash(Sha256.ZeroHash),
    List(Transaction("")),
    MiningTarget.StdMiningTarget,
    Nonce(123L)
  )

  it should "append a correct block" in {
    val blockchains: BlockchainAPI[IO] = setupBlockchainsInterpreter(hashDigests, stubBlockVerification(true))

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      chain = Blockchain(genesis, genesisHash)
      blockToAppend = createBlock(genesisHash)
      appendResult <- blockchains.append(chain, blockToAppend)
    } yield (appendResult, blockToAppend))
      .asserting {
        case (result: Either[Error, Blockchain], appendedBlock: Block) => {
          result.value.blocks shouldBe Vector(genesis, appendedBlock)
          result.value.hashToBlock.values should contain(appendedBlock)
        }
      }
  }

  it should "not append a block when it wasn't verified successfully" in {
    val blockchains: BlockchainAPI[IO] = setupBlockchainsInterpreter(hashDigests, stubBlockVerification(false))

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      chain = Blockchain(genesis, genesisHash)
      blockToAppend = createBlock(genesisHash)
      appendResult <- blockchains.append(chain, blockToAppend)
    } yield appendResult)
      .asserting { _ shouldBe Left(BlockNotVerifiedProperly()) }
  }

  it should "not append a block when the parent hash is wrong" in {
    val blockchains: BlockchainAPI[IO] = setupBlockchainsInterpreter(hashDigests, stubBlockVerification(true))

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      otherBlockHash <- hashDigests.getHashDigest(Bytes(32))
      otherBlock = createBlock(otherBlockHash)
      chain = Blockchain(
        Vector(genesis, otherBlock),
        Map(genesisHash -> genesis, otherBlockHash -> otherBlock)
      )
      blockToAppend = createBlock(genesisHash)
      appendResult <- blockchains.append(chain, blockToAppend)
    } yield appendResult)
      .asserting { _ shouldBe Left(IncorrectParentHash()) }
  }

  it should "not append a block has a non-following index" in {
    val blockchains: BlockchainAPI[IO] = setupBlockchainsInterpreter(hashDigests, stubBlockVerification(true))

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      chain = Blockchain(genesis, genesisHash)
      blockToAppend = createBlock(genesisHash, Index(1000))
      appendResult <- blockchains.append(chain, blockToAppend)
    } yield appendResult)
      .asserting { _ shouldBe Left(InvalidBlockIndex()) }
  }

  it should "not append when there is no parent with a given parent hash" in {
    val blockchains: BlockchainAPI[IO] = setupBlockchainsInterpreter(hashDigests, stubBlockVerification(true))

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      chain = Blockchain(genesis, genesisHash)
      differentHash <- hashDigests.getHashDigest(Bytes(32))
      blockToAppend = createBlock(differentHash, Index(1))
      appendResult <- blockchains.append(chain, blockToAppend)
    } yield appendResult)
      .asserting { _ shouldBe Left(NoSuchParentNodeError()) }
  }

  it should "not find a node with an incorrect index" in {
    val blockchains: BlockchainAPI[IO] = setupBlockchainsInterpreter(hashDigests, stubBlockVerification(true))

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      chain = Blockchain(genesis, genesisHash)
      foundBlock <- blockchains.findByIndex(chain, Index(1000))
    } yield foundBlock)
      .asserting { _ shouldBe None }
  }

  it should "find a node by index" in {
    val blockchains: BlockchainAPI[IO] = setupBlockchainsInterpreter(hashDigests, stubBlockVerification(true))

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      chain = Blockchain(genesis, genesisHash)
      foundBlock <- blockchains.findByIndex(chain, Index(0))
    } yield foundBlock)
      .asserting { _ shouldBe Some(genesis) }
  }

  it should "find a node by hash" in {
    val blockchains: BlockchainAPI[IO] = setupBlockchainsInterpreter(hashDigests, stubBlockVerification(true))

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      chain = Blockchain(genesis, genesisHash)
      foundBlock <- blockchains.findByHash(chain, genesisHash)
    } yield foundBlock)
      .asserting { _ shouldBe Some(genesis) }
  }

  it should "not find a node by hash" in {
    val blockchains: BlockchainAPI[IO] = setupBlockchainsInterpreter(hashDigests, stubBlockVerification(true))

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      differentHash <- hashDigests.getHashDigest(Bytes(32))
      chain = Blockchain(genesis, genesisHash)
      foundBlock <- blockchains.findByHash(chain, differentHash)
    } yield foundBlock)
      .asserting { _ shouldBe None }
  }

  it should "find a common ancestor" in {
    val blockchains: BlockchainAPI[IO] = setupBlockchainsInterpreter(hashDigests, stubBlockVerification(true))

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      commonAncestor = createBlock(genesisHash, Index(1))
      commonAncestorHash <- hashDigests.getHashDigest(serialize(commonAncestor))
      chainA = Blockchain(
        Vector(genesis, commonAncestor),
        Map(
          genesisHash -> genesis,
          commonAncestorHash -> commonAncestor
        )
      )

      extraNode = createBlock(commonAncestorHash, Index(2))
      extraNodeHash <- hashDigests.getHashDigest(serialize(extraNode))

      chainB = Blockchain(
        Vector(genesis, commonAncestor, extraNode),
        Map(
          genesisHash -> genesis,
          commonAncestorHash -> commonAncestor,
          extraNodeHash -> extraNode
        )
      )
      foundCommonAncestor <- blockchains.findCommonAncestor(chainA, chainB)

    } yield (foundCommonAncestor, commonAncestor))
      .asserting {
        case (result, commonAncestor) =>
          result shouldBe Some(commonAncestor)
      }
  }

  it should "not find a common ancestor in a invalid chain" in {
    val blockchains: BlockchainAPI[IO] = setupBlockchainsInterpreter(hashDigests, stubBlockVerification(true))

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))
      chainA = new Blockchain(Vector(), Map())

      extraNode = createBlock(genesisHash, Index(1))
      extraNodeHash <- hashDigests.getHashDigest(serialize(extraNode))

      chainB = Blockchain(
        Vector(genesis, extraNode),
        Map(
          genesisHash -> genesis,
          extraNodeHash -> extraNode
        )
      )
      foundCommonAncestor <- blockchains.findCommonAncestor(chainA, chainB)

    } yield foundCommonAncestor)
      .asserting(_ shouldBe None)
  }

  it should "claim genesis is the common ancestor" in {
    val blockchains: BlockchainAPI[IO] = setupBlockchainsInterpreter(hashDigests, stubBlockVerification(true))

    (for {
      genesisHash <- hashDigests.getHashDigest(serialize(genesis))

      extraNode = createBlock(genesisHash, Index(1))
      extraNodeHash <- hashDigests.getHashDigest(serialize(extraNode))

      anotherNode = createBlock(genesisHash, Index(2))
      anotherNodeHash <- hashDigests.getHashDigest(serialize(extraNode))

      chainA = Blockchain(
        Vector(genesis, extraNode, anotherNode),
        Map(
          genesisHash -> genesis,
          extraNodeHash -> extraNode,
          anotherNodeHash -> anotherNode
        )
      )

      chainB = Blockchain(
        Vector(genesis),
        Map(genesisHash -> genesis)
      )

      foundCommonAncestor <- blockchains.findCommonAncestor(chainA, chainB)

    } yield foundCommonAncestor)
      .asserting(_ shouldBe Some(genesis))
  }

  private def createBlock(parentHash: Hash, index: Index = Index(1)) = {
    Block(
      index,
      parentHash,
      List(Transaction("Some transaction")),
      MiningTarget.StdMiningTarget,
      Nonce(123L)
    )
  }

  private def stubBlockVerification(result: Boolean): BlockVerifier[IO] = new BlockVerifier[IO] {
    override def verify(blockHash: Hash, miningTarget: MiningTarget): IO[Boolean] = IO.pure(result)
  }

  private def setupBlockchainsInterpreter(
    hashDigests: HashProvider[IO],
    blockVerification: BlockVerifier[IO]
  ): BlockchainAPI[IO] = BlockchainAPI.make[IO](blockVerification, hashDigests)
}
