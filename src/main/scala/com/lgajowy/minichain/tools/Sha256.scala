package com.lgajowy.minichain.tools

import com.lgajowy.minichain.tools.BasePrimitives.Bytes

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
