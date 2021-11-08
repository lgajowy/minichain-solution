package com.lgajowy.minichain.algebras

import cats.effect.Sync
import cats.implicits._
import com.lgajowy.minichain.BasePrimitives.Bytes
import com.lgajowy.minichain.domain.Hash

import java.security.MessageDigest

trait HashDigests[F[_]] {

  def zeroHash(): F[Hash]

  def getHashDigest(bytes: Bytes): F[Hash]
}

object HashDigests {

  def makeSHA256[F[_]: Sync](): HashDigests[F] = new HashDigests[F] {

    override final val zeroHash: F[Hash] = getHashDigest(Bytes(32))

    override def getHashDigest(bytes: Array[Byte]): F[Hash] = {
      for {
        messageDigest <- Sync[F].delay { messageDigestInstance() }
        _ = messageDigest.update(bytes)
        digestBytes = messageDigest.digest()
        digest = Hash(digestBytes)
      } yield digest
    }

    private def messageDigestInstance(): MessageDigest = MessageDigest.getInstance("SHA-256")
  }
}
