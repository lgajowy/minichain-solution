package com.lgajowy.minichain.algebras

import cats.data.EitherT
import cats.implicits._
import cats.{ Applicative, Monad }
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.ext.Serializer.serialize

trait BlockchainAPI[F[_]] {
  def append(blockchain: Blockchain, block: Block): F[Either[Error, Blockchain]]

  def findByIndex(blockchain: Blockchain, index: Index): F[Option[Block]]

  def findByHash(blockchain: Blockchain, hash: Hash): F[Option[Block]]
}

object BlockchainAPI {
  def make[F[_]: Monad](
    blockVerification: BlockVerifier[F],
    hashDigests: HashProvider[F]
  ): BlockchainAPI[F] = new BlockchainAPI[F] {

    override def append(blockchain: Blockchain, block: Block): F[Either[Error, Blockchain]] = {
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

    private def getParentFromBlockchain(
      blockchain: Blockchain,
      block: Block
    ): F[Either[NoSuchParentNodeError, Block]] = {
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

    def updateChain(chain: Blockchain, block: Block): F[Either[Error, Blockchain]] = {
      for {
        blockHash <- hashDigests.getHashDigest(serialize(block))
        updatedChain = Blockchain(
          chain.blocks :+ block,
          chain.hashToBlock + (blockHash -> block)
        )
      } yield Right(updatedChain)
    }

    override def findByIndex(blockchain: Blockchain, index: Index): F[Option[Block]] = {
      Applicative[F].pure(blockchain.blocks.get(index.value))
    }

    override def findByHash(blockchain: Blockchain, hash: Hash): F[Option[Block]] =
      Applicative[F].pure(blockchain.hashToBlock.get(hash))
  }

}
