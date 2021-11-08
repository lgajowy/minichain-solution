package com.lgajowy.minichain.algebras

import cats.{ Applicative, Monad }
import com.lgajowy.minichain.domain._
import cats.implicits._
import com.lgajowy.minichain.domain.Block.toBytes

trait Blockchain[F[_]] {

  def append(blockchain: Chain, block: Block): F[Chain]

  def findByIndex(blockchain: Chain, index: Index): F[Option[Block]]

  def findByHash(blockchain: Chain, hash: Hash): F[Option[Block]]

  def findCommonAncestor(chainA: Chain, chainB: Chain): F[Option[Block]]
}

object Blockchain {
  def make[F[_]: Monad](hashProvider: HashProvider[F]): Blockchain[F] = new Blockchain[F] {
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

    private def calculateBlockHash(block: Block): F[Hash] = hashProvider.getHashDigest(toBytes(block))

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
