package com.lgajowy.minichain.algebras

import cats.effect.Sync
import cats.implicits._
import com.lgajowy.minichain.domain.Nonce

import scala.util.Random

trait NonceProvider[F[_]] {
  def getNextNonce(): F[Nonce]
}

object NonceProvider {
  def make[F[_]: Sync](random: Random): NonceProvider[F] = new NonceProvider[F] {
    override def getNextNonce(): F[Nonce] = Sync[F].delay { random.nextLong }.map(Nonce(_))
  }
}
