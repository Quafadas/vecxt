package vecxt

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Float64Array

@js.native
@JSImport("@stdlib/blas/base", JSImport.Default)
object blas extends BlasArrayOps

@js.native
trait BlasArrayOps extends js.Object:
  def daxpy(N: Int, alpha: Double, x: Float64Array, strideX: Int, y: Float64Array, strideY: Int): Unit =
    js.native

  def dscal(N: Int, alpha: Double, x: Float64Array, strideX: Int): Unit = js.native
  def dnrm2(N: Int, x: Float64Array, strideX: Int): Double = js.native
end BlasArrayOps

@js.native
@JSImport("@stdlib/blas/base/dgemm/lib", JSImport.Default)
object dgemm extends js.Object:
  def apply(
      ord: String,
      transA: String,
      transB: String,
      m: Int,
      n: Int,
      k: Int,
      alpha: Double,
      a: Float64Array,
      lda: Int,
      b: Float64Array,
      ldb: Int,
      beta: Double,
      c: Float64Array,
      ldc: Int
  ): Unit = js.native

end dgemm

@js.native
@JSImport("@stdlib/blas/base/dgemv/lib", JSImport.Default)
object dgemv extends js.Object:
  def apply(
      ord: String,
      transA: String,
      m: Int,
      n: Int,
      alpha: Double,
      a: Float64Array,
      lda: Int,
      b: Float64Array,
      ldb: Int,
      beta: Double,
      c: Float64Array,
      ldc: Int
  ): Unit = js.native

end dgemv
