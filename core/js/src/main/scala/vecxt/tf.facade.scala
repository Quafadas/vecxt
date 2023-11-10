package vecxt

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Float64Array

@js.native
@JSImport("@tensorflow/tfjs-core", JSImport.Namespace)
object tf extends ArrayOps

@js.native
trait ArrayOps extends js.Object {
  def add(a: Float64Array, b: Float64Array) : Float64Array = js.native
}