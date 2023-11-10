package vecxt

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Float64Array

@js.native
@JSImport("@stdlib/blas/base", JSImport.Default)
object blas extends ArrayOps

@js.native
trait ArrayOps extends js.Object {
  def daxpy( N: Int, alpha:Double, x: Float64Array, strideX : Int, y: Float64Array, strideY : Int) : Float64Array = js.native
}