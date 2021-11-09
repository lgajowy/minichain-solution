package com.lgajowy.minichain.algebras

import cats.implicits._
import cats.{ Applicative, Monad }
import com.lgajowy.minichain.domain._
import com.lgajowy.minichain.ext.Serializer.serialize

trait Blockchains[F[_]] {

  def append(blockchain: Chain, block: Block): F[Chain]

  def findByIndex(blockchain: Chain, index: Index): F[Option[Block]]

  def findByHash(blockchain: Chain, hash: Hash): F[Option[Block]]

  def findCommonAncestor(chainA: Chain, chainB: Chain): F[Option[Block]]
}

object Blockchains {
  def make[F[_]: Monad](
    hashProvider: HashDigests[F],
  ): Blockchains[F] = new Blockchains[F] {
    override def append(blockchain: Chain, block: Block): F[Chain] = {
      // TODO: verify block
      // TODO: check if index is ok
      // TODO: check if parent hash is ok
      // TODO: check if block is verified against the parent difficulty (Don't use block.difficulty to verify this block)
      //
      // WARNING: use parent's difficulty to verify
      for {
        hash <- calculateBlockHash(block)
        newChain = new Chain(
          block,
          blockchain.indexToHash ++ Map(block.index -> hash),
          blockchain.hashToBlock ++ Map(hash -> block)
        )
      } yield newChain
    }

    private def calculateBlockHash(block: Block): F[Hash] = hashProvider.getHashDigest(serialize(block))

    override def findByIndex(blockchain: Chain, index: Index): F[Option[Block]] = {
      Applicative[F].pure(
        for {
          hash <- blockchain.indexToHash.get(index)
          block <- blockchain.hashToBlock.get(hash)

        } yield block
      )
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
