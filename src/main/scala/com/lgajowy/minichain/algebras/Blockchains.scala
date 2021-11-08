package com.lgajowy.minichain.algebras

import cats.implicits._
import cats.{Applicative, Monad}
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.effects.Serialization

trait Blockchains[F[_]] {

  def append(blockchain: Chain, block: Block): F[Chain]

  def findByIndex(blockchain: Chain, index: Index): F[Option[Block]]

  def findByHash(blockchain: Chain, hash: Hash): F[Option[Block]]

  def findCommonAncestor(chainA: Chain, chainB: Chain): F[Option[Block]]
}

object Blockchains {
  def make[F[_]: Monad: Serialization](hashProvider: HashProvider[F]): Blockchains[F] = new Blockchains[F] {
    override def append(blockchain: Chain, block: Block): F[Chain] = {
      // TODO: verify block
      // TODO: check if index is ok
      // TODO: check if parent hash is ok
      for {
        hash <- calculateBlockHash(block)
        newChain = new Chain(
          block,
          blockchain.indexToHash ++ Map(block.index -> hash),
          blockchain.hashToBlock ++ Map(hash -> block)
        )
      } yield newChain
    }

    private def calculateBlockHash(block: Block): F[Hash] = {
      for {
        blockBytes <- Serialization[F].serialize(block)
        digest <- hashProvider.getHashDigest(blockBytes)
      } yield digest
    }

    override def findByIndex(blockchain: Chain, index: Index): F[Option[Block]] = {
      Applicative[F].pure(
        for {
          hash <- blockchain.indexToHash.get(index)
          block <- blockchain.hashToBlock.get(hash)

        } yield block
      )
    }

    override def findByHash(blockchain: Chain, hash: Hash): F[Option[Block]] =
      Applicative[F].pure(blockchain.hashToBlock.get(hash))

    override def findCommonAncestor(chainA: Chain, chainB: Chain): F[Option[Block]] = ???
  }
}
