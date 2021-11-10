package com.lgajowy.minichain.ext

import com.lgajowy.minichain.base.BasePrimitives.Bytes

import java.security.MessageDigest

object Sha256 {

  final val ZeroHash: Bytes = getHash(Bytes(32))

  def getHash(bytes: Bytes): Bytes = {
    val instance = messageDigestInstance()
    instance.update(bytes)
    instance.digest()
  }

  def messageDigestInstance(): MessageDigest = MessageDigest.getInstance("SHA-256")
}
