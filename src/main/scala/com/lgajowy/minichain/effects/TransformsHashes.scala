package com.lgajowy.minichain.effects

import cats.Applicative
import com.lgajowy.minichain.BasePrimitives.Number
import com.lgajowy.minichain.domain.Hash

trait TransformsHashes[F[_]] {

  def toNumber(hash: Hash): F[Number]

  def toHexString(hash: Hash): F[String]
}

object TransformsHashes {
  def apply[F[_]: TransformsHashes]: TransformsHashes[F] = implicitly

  implicit def make[F[_]: Applicative](): TransformsHashes[F] = new TransformsHashes[F] {
    override def toNumber(hash: Hash): F[Number] = Applicative[F].pure(Number(1, hash.bytes))

    override def toHexString(hash: Hash): F[String] = Applicative[F].pure(toHexString(hash.bytes))

    private def toHexString(bytes: Array[Byte]): String =
      "0x" + bytes.map(b => String.format("%02X", Byte.box(b))).mkString("")
  }
}
