package com.lgajowy.minichain.algebras

import cats.effect.Sync
import com.lgajowy.minichain.domain.Nonce
import cats.implicits._

import scala.util.Random

trait NonceProvider[F[_]] {
  def getNextNonce(): F[Nonce]
}

object NonceProvider {
  def make[F[_]: Sync](seed: Long): NonceProvider[F] = new NonceProvider[F] {

    private val random = new Random(seed)

    override def getNextNonce(): F[Nonce] = Sync[F].delay { random.nextLong }.map(Nonce)
  }
}
