package com.lgajowy.minichain.effects

import cats.effect.Sync

trait Random[F[_]] {
  def nextLong(): F[Long]
}

object Random {
  def apply[F[_]: Random]: Random[F] = implicitly

  implicit def random[F[_]: Sync]: Random[F] = new Random[F] {
    private val randomSource: util.Random = new scala.util.Random()

    override def nextLong(): F[Long] = Sync[F].delay { randomSource.nextLong() }
  }
}
