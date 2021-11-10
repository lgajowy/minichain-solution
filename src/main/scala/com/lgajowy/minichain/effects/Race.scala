package com.lgajowy.minichain.effects

import cats.Foldable
import cats.effect.kernel.Async
import cats.implicits._

trait Race[F[_]] {
  def race[A, B](parallelism: Int, task: F[B]): F[B]
}

object Race {
  def apply[F[_]: Race]: Race[F] = implicitly

  implicit def make[F[_]: Async]: Race[F] = new Race[F] {
    override def race[A, B](parallelism: Int, task: F[B]): F[B] = {
      val tasks: List[F[B]] = (0 until parallelism).map(_ => task).toList
      Foldable[List]
        .foldLeft(tasks.tail, tasks.head) { (acc, next) =>
          Async[F]
            .race(acc, next)
            .map {
              case Right(value) => value
              case Left(value)  => value
            }
        }
    }
  }
}
