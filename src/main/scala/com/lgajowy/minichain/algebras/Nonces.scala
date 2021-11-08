package com.lgajowy.minichain.algebras

import cats.effect.Sync
import cats.implicits._
import com.lgajowy.minichain.domain.Nonce
import com.lgajowy.minichain.effects.Random

trait Nonces[F[_]] {
  def getNextNonce(): F[Nonce]
}

object Nonces {
  def make[F[_]: Sync: Random](): Nonces[F] = new Nonces[F] {
    override def getNextNonce(): F[Nonce] = Random[F].nextLong().map(Nonce)
  }
}
