package com.lgajowy.minichain.algebras

import cats.Functor
import com.lgajowy.minichain.domain.{ Block, Hash }
import cats.implicits._

trait BlockVerifier[F[_]] {
  def verify(block: Block): F[Boolean]
}

object BlockVerifier {

  def make[F[_]: Functor](hashCalculator: HashCalculator[F]): BlockVerifier[F] = new BlockVerifier[F] {
    override def verify(block: Block): F[Boolean] = {

      hashCalculator
        .calculate(block)
        .map((it: Hash) => Hash.toNumber(it) < block.miningTargetNumber.value)
    }

  }
}
