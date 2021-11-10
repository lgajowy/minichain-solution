package com.lgajowy.minichain.algebras

import cats.Functor
import cats.implicits._
import com.lgajowy.minichain.domain.Nonce
import com.lgajowy.minichain.effects.Random

trait NonceProvider[F[_]] {
  def getNextNonce(): F[Nonce]
}

object NonceProvider {
  def make[F[_]: Functor: Random](): NonceProvider[F] = new NonceProvider[F] {
    override def getNextNonce(): F[Nonce] = Random[F].nextLong().map(Nonce)
  }
}
