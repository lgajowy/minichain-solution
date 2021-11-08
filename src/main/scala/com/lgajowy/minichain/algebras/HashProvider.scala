package com.lgajowy.minichain.algebras

import cats.effect.Sync
import cats.implicits._
import com.lgajowy.minichain.Sha256
import com.lgajowy.minichain.domain.Hash

trait HashProvider[F[_]] {

  def zeroHash(): F[Hash]

  def getHashDigest(bytes: Array[Byte]): F[Hash]
}

object HashProvider {

  def makeSHA256[F[_]: Sync](): HashProvider[F] = new HashProvider[F] {
    override final val zeroHash: F[Hash] = Sync[F].pure(Hash(Sha256.ZeroHash))

    override def getHashDigest(bytes: Array[Byte]): F[Hash] = {
      for {
        messageDigest <- Sync[F].delay { Sha256.messageDigestInstance() }
        _ = messageDigest.update(bytes)
        digestBytes = messageDigest.digest()
        digest = Hash(digestBytes)
      } yield digest
    }
  }
}
