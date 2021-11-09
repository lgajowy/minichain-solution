package com.lgajowy.minichain.algebras

import cats.effect.Sync
import cats.implicits._
import com.lgajowy.minichain.base.BasePrimitives.Bytes
import com.lgajowy.minichain.domain.Hash
import com.lgajowy.minichain.ext.Sha256

trait HashDigests[F[_]] {
  def getHashDigest(bytes: Bytes): F[Hash]
}

object HashDigests {

  def makeSHA256[F[_]: Sync](): HashDigests[F] = new HashDigests[F] {

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
