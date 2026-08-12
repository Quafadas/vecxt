package vecxt
import scala.scalanative.unsafe.*

import org.ekrich.blas.unsafe.blas
import org.ekrich.blas.unsafe.blasEnums

import vecxt.MatrixInstance.*
import vecxt.matrix.*

object NativeDoubleMatrix:
  extension (m: Matrix[Double])
    def *:*(bmat: Matrix[Boolean]): Matrix[Double] =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        val newArr = Array.ofDim[Double](m.rows * m.cols)
        var i = 0
        while i < newArr.length do
          newArr(i) = if bmat.raw(i) then m.raw(i) else 0.0
          i += 1
        end while
        Matrix[Double](newArr, m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val bIdx = bmat.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = if bmat.raw(bIdx) then m.raw(mIdx) else 0.0
        }
        Matrix[Double](newArr, m.rows, m.cols)
      end if
    end *:*

    def *:*=(bmat: Matrix[Boolean]): Unit =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        var i = 0
        while i < m.raw.length do
          if !bmat.raw(i) then m.raw(i) = 0.0
          end if
          i += 1
        end while
      else
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val bIdx = bmat.layout.linearIndex(i, j)
          if !bmat.raw(bIdx) then m.raw(mIdx) = 0.0
          end if
        }
      end if
    end *:*=

    def +=(arr: Array[Double]): Unit =
      assert(arr.length == m.cols, s"Array length ${arr.length} != expected ${m.cols}")

      m.layout.foreach2D { (i, j) =>
        m(i, j) = m(i, j) + arr(j)
      }

    end +=

    def +=(n: Double): Unit =

      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.+=(m.raw)(n)
      else
        // Cache-friendly fallback: iterate with smallest stride in inner loop
        if m.rowStride <= m.colStride then
          // Row stride is smaller, so iterate rows in inner loop
          var j = 0
          while j < m.cols do
            var i = 0
            while i < m.rows do
              m(i, j) = n + m(i, j)
              i += 1
            end while
            j += 1
          end while
        else
          // Column stride is smaller, so iterate columns in inner loop
          var i = 0
          while i < m.rows do
            var j = 0
            while j < m.cols do
              m(i, j) = n + m(i, j)
              j += 1
            end while
            i += 1
          end while
        end if
      end if

    end +=

    def >=(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](doublearrays.>=(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) >= d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    def >(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](doublearrays.>(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) > d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)
      end if
    end >

    def <=(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](doublearrays.<=(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) <= d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)
      end if
    end <=

    def <(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](doublearrays.<(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) < d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    /** Writes `alpha * (m @@ b) + beta * c` into `c` in place, via `cblas_dgemm`.
      *
      * `c` must already be shaped `(m.rows, b.cols)` and dense column-major — `ldc` is hardcoded to `m.rows` below, and
      * `dgemm` also reads `c` when `beta != 0`, so any other shape or layout would be silently written to (or read
      * from) incorrectly rather than rejected. Use `matmul`/`@@` instead if you don't already have a conforming `c` to
      * write into; they allocate one for you.
      */
    def `matmulInPlace!`(
        b: Matrix[Double],
        c: Matrix[Double],
        alpha: Double = 1.0,
        beta: Double = 0.0
    ): Unit =
      dimMatCheck(m, b)
      matmulOutputCheck(m, b, c)

      if m.hasSimpleContiguousMemoryLayout && b.hasSimpleContiguousMemoryLayout then
        val lda = if m.isDenseColMajor then m.rows else m.cols
        val ldb = if b.isDenseColMajor then b.rows else b.cols
        val transB = if b.isDenseColMajor then blasEnums.CblasNoTrans else blasEnums.CblasTrans
        val transA = if m.isDenseColMajor then blasEnums.CblasNoTrans else blasEnums.CblasTrans

        // `order` is always CblasColMajor here, deliberately, even when both operands are dense row-major: transA/
        // transB/lda/ldb above are the standard "always column-major" transpose trick. Switching order to
        // CblasRowMajor without also inverting transA/transB would apply the transpose trick twice — see the
        // matching comment in src-js/doublematrix.scala, which hits the identical order/trans interaction.
        blas.cblas_dgemm(
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
      else if (m.rowStride == 1 || m.colStride == 1) && (b.rowStride == 1 || b.colStride == 1) then
        val transB = if b.rowStride == 1 then blasEnums.CblasNoTrans else blasEnums.CblasTrans
        val transA = if m.rowStride == 1 then blasEnums.CblasNoTrans else blasEnums.CblasTrans
        // See the fully-dense branch above: order stays CblasColMajor regardless of m/b's own orientation.
        blas.cblas_dgemm(
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

    /** Writes `alpha * (m @@ vec) + beta * y` into `y` in place, via CBLAS `cblas_dgemv`. Native counterpart of
      * `JvmDoubleMatrix.*=`; see there for the reasoning, which carries over with one simplification.
      *
      * CBLAS takes an `order` argument, so unlike the Fortran interface on the JVM there is no need to hand the
      * dimensions over transposed: a `colStride == 1` layout is described directly as `CblasRowMajor` with
      * `lda = rowStride`, and a `rowStride == 1` one as `CblasColMajor` with `lda = colStride`. Both keep
      * `CblasNoTrans` and the natural `(rows, cols)`.
      *
      * `lda` was previously `m.rows` for both orders. That is only right for column-major: under `CblasRowMajor`, `lda`
      * is the distance between successive rows and must be at least `cols`, so a non-square dense row-major matrix —
      * `m.transpose` of any non-square matrix, for instance — was being read with the wrong stride. Taking it from the
      * layout fixes that and generalises to padded strides at the same time.
      *
      * Offsets need no fallback here: `raw.at(offset)` is a pointer into the middle of the array, which is exactly what
      * CBLAS wants.
      *
      * @param vec
      *   the vector to multiply by; must have length `m.cols`
      * @param y
      *   the destination, accumulated onto per `beta`; must have length `m.rows`
      */
    def *=(vec: Array[Double], y: Array[Double], alpha: Double = 1.0, beta: Double = 1.0): Unit =
      if vec.length != m.cols then
        throw new IllegalArgumentException(s"Vector length ${vec.length} != expected ${m.cols}")
      end if
      if y.length != m.rows then
        throw new IllegalArgumentException(s"Destination length ${y.length} != expected ${m.rows}")
      end if
      val nonEmpty = m.rows > 0 && m.cols > 0

      if nonEmpty && m.rowStride == 1 && m.colStride >= m.rows then
        blas.cblas_dgemv(
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
        blas.cblas_dgemv(
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
          var acc = 0.0
          var j = 0
          while j < m.cols do
            acc += m.raw(m.layout.linearIndex(i, j)) * vec(j)
            j += 1
          end while
          y(i) = if beta == 0.0 then alpha * acc else alpha * acc + beta * y(i)
          i += 1
        end while
      end if
    end *=

    /** Matrix-vector product: returns `alpha * (m @@ vec)` as a fresh array. Wrapper over [[*=]] with `beta = 0`, so
      * the freshly allocated destination is written without being read. See `JvmDoubleMatrix.*` for why there is no
      * `beta` parameter here.
      */
    def *(vec: Array[Double], alpha: Double = 1.0): Array[Double] =
      val out = Array.ofDim[Double](m.rows)
      m.*=(vec, out, alpha, 0.0)
      out
    end *
  end extension

end NativeDoubleMatrix

object JvmDoubleMatrix:

end JvmDoubleMatrix
