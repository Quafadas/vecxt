package vecxt
import scala.scalanative.unsafe.*

import org.ekrich.blas.unsafe.blas
import org.ekrich.blas.unsafe.blasEnums

import vecxt.MatrixInstance.*
import vecxt.matrix.*
import scala.annotation.targetName

object NativeFloatMatrix:

  extension (m: Matrix[Float])

    @targetName("matmulFloat")
    def @@(b: Matrix[Float]): Matrix[Float] =
      m.matmul(b, 1.0f, 0.0f)

    @targetName("matmulFloatNonDefault")
    def matmul(b: Matrix[Float], alpha: Float, beta: Float): Matrix[Float] =
      dimMatCheck(m, b)
      val newArr: Array[Float] = Array.ofDim[Float](m.rows * b.cols)
      val newmat = Matrix[Float](newArr, m.rows, b.cols)
      m.`matmulInPlace!`(b, newmat, alpha, beta)
      newmat
    end matmul

    /** Writes `alpha * (m @@ b) + beta * c` into `c` in place, via `cblas_sgemm`. Float counterpart of
      * `NativeDoubleMatrix.matmulInPlace!`; see there for the reasoning behind the `trans`/`order` choices, which
      * carries over unchanged aside from element type.
      *
      * `c` must already be shaped `(m.rows, b.cols)` and dense column-major — `ldc` is hardcoded to `m.rows` below, and
      * `sgemm` also reads `c` when `beta != 0`, so any other shape or layout would be silently written to (or read
      * from) incorrectly rather than rejected. Use `matmul`/`@@` instead if you don't already have a conforming `c` to
      * write into; they allocate one for you.
      */
    @targetName("matmulFloatInPlace")
    def `matmulInPlace!`(b: Matrix[Float], c: Matrix[Float], alpha: Float, beta: Float): Unit =
      dimMatCheck(m, b)
      matmulOutputCheck(m, b, c)

      if m.hasSimpleContiguousMemoryLayout && b.hasSimpleContiguousMemoryLayout then
        val lda = if m.isDenseColMajor then m.rows else m.cols
        val ldb = if b.isDenseColMajor then b.rows else b.cols
        val transB = if b.isDenseColMajor then blasEnums.CblasNoTrans else blasEnums.CblasTrans
        val transA = if m.isDenseColMajor then blasEnums.CblasNoTrans else blasEnums.CblasTrans

        // See NativeDoubleMatrix.matmulInPlace! for why `order` is always CblasColMajor here.
        blas.cblas_sgemm(
          blasEnums.CblasColMajor,
          transA,
          transB,
          m.rows,
          b.cols,
          m.cols,
          alpha,
          m.raw.at(0),
          lda,
          b.raw.at(0),
          ldb,
          beta,
          c.raw.at(0),
          m.rows
        )
      else if blasLeadingDimensionCheck(m) && blasLeadingDimensionCheck(b) then
        val transB = if b.rowStride == 1 then blasEnums.CblasNoTrans else blasEnums.CblasTrans
        val transA = if m.rowStride == 1 then blasEnums.CblasNoTrans else blasEnums.CblasTrans
        // See the fully-dense branch above: order stays CblasColMajor regardless of m/b's own orientation.
        blas.cblas_sgemm(
          blasEnums.CblasColMajor,
          transA,
          transB,
          m.rows,
          b.cols,
          m.cols,
          alpha,
          m.raw.at(m.offset),
          if m.rowStride == 1 then m.colStride else m.rowStride,
          b.raw.at(b.offset),
          if b.rowStride == 1 then b.colStride else b.rowStride,
          beta,
          c.raw.at(c.offset),
          m.rows
        )
      else
        throw UnsupportedLayoutException(
          s"matmulInPlace! does not support this combination of matrix layouts. m: ${m.layoutString}, b: ${b.layoutString}"
        )

      end if
    end `matmulInPlace!`

    /** Writes `alpha * (m @@ vec) + beta * y` into `y` in place, via CBLAS `cblas_sgemv`. Native counterpart of
      * `JvmFloatMatrix.*=`; see `JvmDoubleMatrix.*=` for the reasoning behind the guards.
      *
      * Mirrors `NativeDoubleMatrix.*=` exactly: CBLAS takes an `order` argument, so a `colStride == 1` layout is
      * described directly as `CblasRowMajor` with `lda = rowStride` and a `rowStride == 1` one as `CblasColMajor` with
      * `lda = colStride`, both keeping `CblasNoTrans` and the natural `(rows, cols)`. Offsets need no fallback, because
      * `raw.at(offset)` is a pointer into the middle of the array, which is what CBLAS wants.
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
