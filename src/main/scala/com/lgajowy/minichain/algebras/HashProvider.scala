package com.lgajowy.minichain.algebras

import cats.effect.Sync
import cats.implicits._
import com.lgajowy.minichain.base.BasePrimitives.Bytes
import com.lgajowy.minichain.domain.Hash
import com.lgajowy.minichain.ext.Sha256

trait HashProvider[F[_]] {
  def getHashDigest(bytes: Bytes): F[Hash]
}

object HashProvider {

  def makeSHA256[F[_]: Sync](): HashProvider[F] = new HashProvider[F] {

    override def getHashDigest(bytes: Bytes): F[Hash] = {
      for {
        messageDigest <- Sync[F].delay { Sha256.messageDigestInstance() }
        _ = messageDigest.update(bytes)
        digestBytes = messageDigest.digest()
        digest = Hash(digestBytes)
      } yield digest
    }
  }
}
