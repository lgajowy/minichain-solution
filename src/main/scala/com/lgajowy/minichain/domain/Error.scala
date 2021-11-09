package com.lgajowy.minichain.domain

import scala.util.control.NoStackTrace

sealed trait Error extends NoStackTrace
case class NoSuchParentNodeError() extends Error
case class BlockNotVerifiedProperly() extends Error
case class InvalidBlockIndex() extends Error
case class IncorrectParentHash() extends Error
