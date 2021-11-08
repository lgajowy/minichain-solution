package com.lgajowy.minichain.algebras

import cats.Applicative
import com.lgajowy.minichain.domain.Hash
import com.lgajowy.minichain.BasePrimitives.Number

trait HashTransformer[F[_]] {

  def toNumber(hash: Hash): F[BigInt]

  def toHexString(hash: Hash): F[String]
}

object HashTransformer {
  def make[F[_]: Applicative](): HashTransformer[F] = new HashTransformer[F] {
    override def toNumber(hash: Hash): F[BigInt] = Applicative[F].pure(Number(1, hash.bytes))

    override def toHexString(hash: Hash): F[String] = Applicative[F].pure(toHexString(hash.bytes))

    private def toHexString(bytes: Array[Byte]): String =
      "0x" + bytes.map(b => String.format("%02X", Byte.box(b))).mkString("")
  }
}
