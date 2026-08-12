package vecxt

import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.Float32Array

import vecxt.MatrixInstance.*
import vecxt.matrix.*
import scala.annotation.targetName

object JsFloatMatrix:

  extension (m: Matrix[Float])

    /** Writes `alpha * (m @@ vec) + beta * y` into `y` in place, via the stdlib `sgemv` shim. JS counterpart of
      * `JvmFloatMatrix.*=`; see `JvmDoubleMatrix.*=` for the reasoning behind the guards.
      *
      * Like the `Double` shim, `sgemv` takes an `order` argument, so a layout is stated rather than corrected for: a
      * `colStride == 1` operand is `"row-major"` with `lda = rowStride`, a `rowStride == 1` one is `"column-major"`
      * with `lda = colStride`, and both keep `"no-transpose"` and the natural `(rows, cols)`.
      *
      * Offset views take the elementwise branch, as they do for `Double` here: the facade has no offset parameter and
      * slicing to fake one would add another copy to a path that already marshals the whole backing array into a
      * `Float32Array` and the result back out.
      *
      * @param vec
      *   the vector to multiply by; must have length `m.cols`
      * @param y
      *   the destination, accumulated onto per `beta`; must have length `m.rows`
      */
    def *=(vec: Array[Float], y: Array[Float], alpha: Float, beta: Float): Unit =
      if vec.length != m.cols then
        throw new IllegalArgumentException(s"Vector length ${vec.length} != expected ${m.cols}")
      end if
      if y.length != m.rows then
        throw new IllegalArgumentException(s"Destination length ${y.length} != expected ${m.rows}")
      end if
      val nonEmpty = m.rows > 0 && m.cols > 0
      val asColMajor = nonEmpty && m.offset == 0 && m.rowStride == 1 && m.colStride >= m.rows
      val asRowMajor = nonEmpty && m.offset == 0 && m.colStride == 1 && m.rowStride >= m.cols

      if asColMajor || asRowMajor then
        val yBuf = new Float32Array(y.toJSArray)
        sgemv(
          if asColMajor then "column-major" else "row-major",
          "no-transpose",
          m.rows,
          m.cols,
          alpha,
          new Float32Array(m.raw.toJSArray),
          if asColMajor then m.colStride else m.rowStride,
          new Float32Array(vec.toJSArray),
          1,
          beta,
          yBuf,
          1
        )
        var i = 0
        while i < m.rows do
          y(i) = yBuf(i)
          i += 1
        end while
      else
        var i = 0
        while i < m.rows do
          var acc = 0.0f
          var j = 0
          while j < m.cols do
            acc += m.raw(m.layout.linearIndex(i, j)) * vec(j)
            j += 1
          end while
          y(i) = if beta == 0.0f then alpha * acc else alpha * acc + beta * y(i)
          i += 1
        end while
      end if
    end *=

    /** Matrix-vector product: returns `m @@ vec` as a fresh array. Wrapper over [[*=]] with `beta = 0`. Two arities
      * rather than a defaulted `alpha`, because `all` permits only one overload of `*` to carry defaults and the
      * `Double` one already does.
      */
    @targetName("matmulFloatVector")
    def *(vec: Array[Float]): Array[Float] = m.*(vec, 1.0f)

    @targetName("matmulFloatVectorScaled")
    def *(vec: Array[Float], alpha: Float): Array[Float] =
      val out = Array.ofDim[Float](m.rows)
      m.*=(vec, out, alpha, 0.0f)
      out
    end *

  end extension
end JsFloatMatrix

/** Cross-compilation stub — the Native implementation lives in `src-native/floatmatrix.scala`. `all` exports this name
  * on every platform, so it has to exist on all of them.
  */
object NativeFloatMatrix
