package com.lgajowy.minichain.ext

import com.lgajowy.minichain.base.BasePrimitives.Bytes
import com.lgajowy.minichain.domain.Hash

import java.security.MessageDigest

// Hashes are produced by a cryptographic function and in "mini-chain" we
// use SHA-256, which always generates a 32-byte (256-bit) value.
object Sha256 {
  val NumberOfBytes = 32
  val TheDigest = MessageDigest.getInstance(???)

  // We pre-compute the hash of an empty array of 32 bytes.
  // We call this the "Zero_Hash".
  val ZeroHash: Hash = ???

  // We use this to hash a composite structure whose constituents can be given
  // as byte arrays. We just feed everything to SHA-256.
  def apply(bytess: Bytes*): Hash = {
    for (bytes <- bytess) {
      TheDigest.update(bytes)
    }

    val hash = TheDigest.digest()
    assert(hash.length == NumberOfBytes)

    Hash(hash)
  }
}
