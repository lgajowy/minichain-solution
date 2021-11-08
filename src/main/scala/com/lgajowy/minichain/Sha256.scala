package com.lgajowy.minichain

import com.lgajowy.minichain.BasePrimitives.Bytes

import java.security.MessageDigest

object Sha256 {

  final val NumberOfBytes = 32

  final val ZeroHash: Bytes = {
    val instance = messageDigestInstance()
    instance.update(Bytes(32))
    instance.digest()
  }

  def messageDigestInstance(): MessageDigest = MessageDigest.getInstance("SHA-256")
}
