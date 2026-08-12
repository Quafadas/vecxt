package vecxt
import scala.scalanative.unsafe.*

import org.ekrich.blas.unsafe.blas
import org.ekrich.blas.unsafe.blasEnums

import vecxt.MatrixInstance.*
import vecxt.matrix.*
import scala.annotation.targetName

object NativeFloatMatrix:

  extension (m: Matrix[Float])

    /** Writes `alpha * (m @@ vec) + beta * y` into `y` in place, via CBLAS `cblas_sgemv`. Native counterpart of
      * `JvmFloatMatrix.*=`; see `JvmDoubleMatrix.*=` for the reasoning behind the guards.
      *
      * Mirrors `NativeDoubleMatrix.*=` exactly: CBLAS takes an `order` argument, so a `colStride == 1` layout is
      * described directly as `CblasRowMajor` with `lda = rowStride` and a `rowStride == 1` one as `CblasColMajor`
      * with `lda = colStride`, both keeping `CblasNoTrans` and the natural `(rows, cols)`. Offsets need no fallback,
      * because `raw.at(offset)` is a pointer into the middle of the array, which is what CBLAS wants.
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

      if nonEmpty && m.rowStride == 1 && m.colStride >= m.rows then
        blas.cblas_sgemv(
          blasEnums.CblasColMajor,
          blasEnums.CblasNoTrans,
          m.rows,
          m.cols,
          alpha,
          m.raw.at(m.offset),
          m.colStride,
          vec.at(0),
          1,
          beta,
          y.at(0),
          1
        )
      else if nonEmpty && m.colStride == 1 && m.rowStride >= m.cols then
        blas.cblas_sgemv(
          blasEnums.CblasRowMajor,
          blasEnums.CblasNoTrans,
          m.rows,
          m.cols,
          alpha,
          m.raw.at(m.offset),
          m.rowStride,
          vec.at(0),
          1,
          beta,
          y.at(0),
          1
        )
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
end NativeFloatMatrix

/** Cross-compilation stub — the JS implementation lives in `src-js/floatmatrix.scala`. `all` exports this name on every
  * platform, so it has to exist on all of them.
  */
object JsFloatMatrix
