package com.lgajowy.minichain.tools

import com.lgajowy.minichain.domain.Base.Bytes
import com.lgajowy.minichain.domain.Hash

import java.security.MessageDigest

object Sha256 {
  private val algorithm = "SHA-256"
  private val numberOfBytes = 32

  val zeroHash: Hash = {
    val instance = getDigestMessageInstance()
    instance.update(Bytes(32))
    Hash(instance.digest())
  }

  // TODO: Is this thread safe now, that we get a new MessageDigest every time?
  //  Is this memory efficient (can I do better)?
  def apply(aggregatedBytes: Bytes*): Hash = {
    val messageDigest: MessageDigest = getDigestMessageInstance()
    val allBytes: Array[Byte] = aggregatedBytes.flatten.toArray
    messageDigest.update(allBytes)

    val hash = messageDigest.digest()

    // TODO: Do I need this assertion anyway?
    assert(hash.length == numberOfBytes)

    Hash(hash)
  }

  private def getDigestMessageInstance(): MessageDigest = MessageDigest.getInstance(algorithm)
}
