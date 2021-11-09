package com.lgajowy.minichain.domain

case class Chain(blocks: Vector[Block], hashToBlock: Map[Hash, Block])
