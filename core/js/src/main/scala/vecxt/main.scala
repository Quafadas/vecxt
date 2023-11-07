import scala.scalajs.js.typedarray.Float64Array

import scala.util.chaining.*
import vecxt.*

@main def checkBytecode =
  val a = Float64Array(3).tap(_.fill(1.0))
  val a1 = Float64Array(3).tap(_.fill(2.0))
  // val b = Array[Boolean](true, false, true)
  // val c = Array[Boolean](false, true, true)

  import vecxt.BoundsCheck.yes

  a - a1