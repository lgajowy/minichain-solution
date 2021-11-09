package com.lgajowy.minichain.algebras

import cats.data.EitherT
import cats.implicits._
import cats.{ Applicative, Monad }
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.ext.Serializer.serialize

trait Blockchains[F[_]] {
  def append(blockchain: Chain, block: Block): F[Either[Error, Chain]]

  def findByIndex(blockchain: Chain, index: Index): F[Option[Block]]

  def findByHash(blockchain: Chain, hash: Hash): F[Option[Block]]

  def findCommonAncestor(chainA: Chain, chainB: Chain): F[Option[Block]]
}

object Blockchains {
  def make[F[_]: Monad](
    blockVerification: BlockVerification[F],
    hashDigests: HashDigests[F]
  ): Blockchains[F] = new Blockchains[F] {

    override def append(blockchain: Chain, block: Block): F[Either[Error, Chain]] = {
      val parent: F[Either[NoSuchParentNodeError, Block]] = getParentFromBlockchain(blockchain, block)

      (for {
        parent <- EitherT(parent)
        lastChainBlock = blockchain.blocks.last
        _ <- EitherT(checkParentHashCorrectness(lastChainBlock, block))
        _ <- EitherT(checkIndexCorrectness(lastChainBlock, block))
        _ <- EitherT(verifyBlock(block, parent.miningTarget))
        updatedChain <- EitherT(updateChain(blockchain, block))
      } yield updatedChain).value
    }

    private def getParentFromBlockchain(blockchain: Chain, block: Block): F[Either[NoSuchParentNodeError, Block]] = {
      Applicative[F].pure {
        blockchain.hashToBlock.get(block.parentHash) match {
          case Some(block) => Right(block)
          case None        => Left(NoSuchParentNodeError())
        }
      }
    }

    def verifyBlock(block: Block, parentMiningTarget: MiningTarget): F[Either[BlockNotVerifiedProperly, Unit]] = {
      for {
        blockHash <- hashDigests.getHashDigest(serialize(block))
        isVerified <- blockVerification.verify(blockHash, parentMiningTarget)
        result = if (!isVerified) Left(BlockNotVerifiedProperly()) else Right()
      } yield result
    }

    def checkIndexCorrectness(lastChainBlock: Block, block: Block): F[Either[InvalidBlockIndex, Unit]] =
      Monad[F].pure {
        if (lastChainBlock.index.value == block.index.value - 1) {
          Right()
        } else {
          Left(InvalidBlockIndex())
        }
      }

    def checkParentHashCorrectness(lastChainBlock: Block, block: Block): F[Either[IncorrectParentHash, Unit]] = {
      hashDigests
        .getHashDigest(serialize(lastChainBlock))
        .map { realParentHash =>
          {
            if (realParentHash.toNumber() == block.parentHash.toNumber()) {
              Right()
            } else {
              Left(IncorrectParentHash())
            }
          }
        }
    }

    def updateChain(chain: Chain, block: Block): F[Either[Error, Chain]] = {
      for {
        blockHash <- hashDigests.getHashDigest(serialize(block))
        updatedChain = Chain(
          chain.blocks :+ block,
          chain.hashToBlock + (blockHash -> block)
        )
      } yield Right(updatedChain)
    }

    override def findByIndex(blockchain: Chain, index: Index): F[Option[Block]] = {
      Applicative[F].pure(blockchain.blocks.get(index.value))
    }

    // TODO: mention tries (git hash saving mechanism)
    override def findByHash(blockchain: Chain, hash: Hash): F[Option[Block]] =
      Applicative[F].pure(blockchain.hashToBlock.get(hash))

    // TODO: genesis is common ancestor
    // TODO: test what happens if there is no common ancestor at all (different genesis blocks). (fake blockchain).
    // TODO: other node is common ancestor
    override def findCommonAncestor(chainA: Chain, chainB: Chain): F[Option[Block]] = {
        Applicative[F].pure(findCommonAncestorBlock(chainA, chainB))
    }
  }

  // TODO: optimize
  private def findCommonAncestorBlock(chainA: Chain, chainB: Chain): Option[Block] = {
    val aBlocks = chainA.blocks
    val bBlocks = chainB.blocks

    val aIsLonger = aBlocks.length >= bBlocks.length
    val longer = if (aIsLonger) aBlocks else bBlocks
    val shorter = if (aIsLonger) bBlocks else aBlocks

    val a = longer.take(shorter.length)
    val b = shorter
    val lastIndex = a.length -1

    for (i <- lastIndex to 0 by -1) {
      if (a(i) == b(i)) {
        return Some(a(i))
      }
    }
    None
  }
}
