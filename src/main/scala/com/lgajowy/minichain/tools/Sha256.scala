package com.lgajowy.minichain.tools

import BasePrimitives.Bytes

import java.security.MessageDigest

object Sha256 {
  private val algorithm = "SHA-256"

  final val NumberOfBytes = 32

  final val ZeroHash: Bytes = {
    val instance = getDigestMessageInstance()
    instance.update(Bytes(32))
    instance.digest()
  }

  // TODO: Is this thread safe now, that we get a new MessageDigest every time?
  //  Is this memory efficient (can I do better)?
  def apply(aggregatedBytes: Bytes*): Bytes = {
    val messageDigest: MessageDigest = getDigestMessageInstance()
    val allBytes: Array[Byte] = aggregatedBytes.flatten.toArray
    messageDigest.update(allBytes)

    val hash = messageDigest.digest()

    // TODO: Do I need this assertion anyway?
    assert(hash.length == NumberOfBytes)
    hash
  }

  private def getDigestMessageInstance(): MessageDigest = MessageDigest.getInstance(algorithm)
}
