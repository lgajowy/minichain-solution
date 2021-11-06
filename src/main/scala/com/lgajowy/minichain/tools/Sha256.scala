package com.lgajowy.minichain.tools

import com.lgajowy.minichain.domain.Base.Bytes
import com.lgajowy.minichain.domain.Hash

import java.security.MessageDigest

object Sha256 {

  val NumberOfBytes = 32
  val TheDigest: MessageDigest = MessageDigest.getInstance(???)

  val Zero_Hash: Hash = ???

  def apply(aggregatedBytes: Bytes*): Hash = {

    for (bytes <- aggregatedBytes) {
      TheDigest.update(bytes)
    }

    val hash = TheDigest.digest()
    assert(hash.length == NumberOfBytes)

    Hash(hash)
  }
}