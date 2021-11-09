package com.lgajowy.minichain.algebras

import cats.data.EitherT
import cats.implicits._
import cats.{ Applicative, Monad }
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.ext.Serializer.serialize

import scala.util.control.NoStackTrace

sealed trait Error extends NoStackTrace
case class NoParentNodeError() extends Error
case class BlockNotVerifiedProperly() extends Error
case class InvalidBlockIndex() extends Error
case class ParentHashInvalid() extends Error

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
      val parent: F[Either[NoParentNodeError, Block]] = Monad[F].pure {
        blockchain.hashToBlock.get(block.parentHash) match {
          case Some(block) => Right(block)
          case None        => Left(NoParentNodeError())
        }
      }

      def verifyBlock(block: Block, parentMiningTarget: MiningTarget): F[Either[Error, Unit]] = {
        val blockBytes = serialize(block)
        for {
          blockHash <- hashDigests.getHashDigest(blockBytes)
          isVerified <- blockVerification.verify(blockBytes, blockHash, parentMiningTarget)
          result = if (!isVerified) Left(BlockNotVerifiedProperly()) else Right()
        } yield result
      }

      def lastChainBlock(chain: Chain): F[Either[Error, Block]] = Monad[F].pure {
        chain.blocks.lastOption match {
          case Some(value) => Right(value)
          case None        => Left(NoParentNodeError())
        }
      }

      def checkIndexCorrectness(lastChainBlock: Block, block: Block): F[Either[Error, Unit]] = Monad[F].pure {
        if (lastChainBlock.index.value == block.index.value - 1) {
          Right()
        } else {
          Left(InvalidBlockIndex())
        }
      }

      def checkParentHashCorrectness(lastChainBlock: Block, block: Block): F[Either[Error, Unit]] = Monad[F].pure {
        if (lastChainBlock.parentHash == block.parentHash) {
          Right()
        } else {
          Left(ParentHashInvalid())
        }
      }

      (for {
        parent <- EitherT(parent)
        lastChainBlock <- EitherT(lastChainBlock(blockchain))
        _ <- EitherT(checkParentHashCorrectness(lastChainBlock, block))
        _ <- EitherT(checkIndexCorrectness(lastChainBlock, block))
        _ <- EitherT(verifyBlock(block, parent.miningTarget))
        updatedChain = Chain(
          blockchain.blocks :+ block,
          blockchain.hashToBlock
        )

      } yield updatedChain).value
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
    override def findCommonAncestor(chainA: Chain, chainB: Chain): F[Option[Block]] = ???
  }
}
