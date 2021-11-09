package com.lgajowy.minichain.algebras

import cats.effect.Sync
import cats.implicits._
import com.lgajowy.minichain.base.BasePrimitives.Bytes
import com.lgajowy.minichain.domain.Hash

import java.security.MessageDigest

trait HashDigests[F[_]] {

  val zeroHash: Hash

  def getHashDigest(bytes: Bytes): F[Hash]
}

object HashDigests {

  def makeSHA256[F[_]: Sync](): HashDigests[F] = new HashDigests[F] {

    override final val zeroHash: Hash = getHash(Bytes(32))

    override def getHashDigest(bytes: Bytes): F[Hash] = {
      for {
        messageDigest <- Sync[F].delay { messageDigestInstance() }
        _ = messageDigest.update(bytes)
        digestBytes = messageDigest.digest()
        digest = Hash(digestBytes)
      } yield digest
    }

    private def getHash(bytes: Bytes): Hash = {
      val instance = messageDigestInstance()
      instance.update(bytes)
      Hash(instance.digest())
    }

    private def messageDigestInstance(): MessageDigest = MessageDigest.getInstance("SHA-256")
  }
}
