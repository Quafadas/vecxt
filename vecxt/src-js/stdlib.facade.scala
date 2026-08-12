package vecxt

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Float32Array
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

  def sdot(N: Int, x: Float64Array, strideX: Int, y: Float64Array, strideY: Int): Double = js.native
  def saxpy(
      N: Int,
      alpha: Float,
      x: js.typedarray.Float32Array,
      strideX: Int,
      y: js.typedarray.Float32Array,
      strideY: Int
  ): Unit =
    js.native
  def sscal(N: Int, alpha: Float, x: js.typedarray.Float32Array, strideX: Int): Unit = js.native
  def snrm2(N: Int, x: js.typedarray.Float32Array, strideX: Int): Float = js.native
  def sdot(N: Int, x: js.typedarray.Float32Array, strideX: Int, y: js.typedarray.Float32Array, strideY: Int): Float =
    js.native

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

/** `sgemv( order, trans, M, N, α, A, LDA, x, sx, β, y, sy )` — `y = α*A*x + β*y`, or `y = α*Aᵀ*x + β*y` when `trans`
  * selects the transpose.
  *
  * Single-precision twin of [[dgemv]], and the same shape: `order` describes how `A` is laid out in memory, so vecxt
  * passes `"no-transpose"` throughout and expresses a row- or column-major operand through `order`/`lda` instead. The
  * parameter names below follow stdlib's own signature — `x`/`sx` and `y`/`sy` are the vector and its stride, not a
  * second and third matrix, which the `b`/`ldb`/`c`/`ldc` naming carried over into [[dgemv]] rather obscures.
  */
@js.native
@JSImport("@stdlib/blas/base/sgemv/lib", JSImport.Default)
object sgemv extends js.Object:
  def apply(
      order: String,
      trans: String,
      m: Int,
      n: Int,
      alpha: Float,
      a: Float32Array,
      lda: Int,
      x: Float32Array,
      sx: Int,
      beta: Float,
      y: Float32Array,
      sy: Int
  ): Unit = js.native

end sgemv
