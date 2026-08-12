package vecxt

import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.Float64Array

import vecxt.MatrixInstance.*
import vecxt.matrix.*

object JsDoubleMatrix:

  extension (m: Matrix[Double])

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

    /** Writes `alpha * (m @@ b) + beta * c` into `c` in place, via a JS `dgemm` shim.
      *
      * `c` must already be shaped `(m.rows, b.cols)` and dense column-major — `ldc` is hardcoded to `m.rows` below, and
      * `dgemm` also reads `c` when `beta != 0`, so any other shape or layout would be silently written to (or read
      * from) incorrectly rather than rejected. Use `matmul`/`@@` instead if you don't already have a conforming `c` to
      * write into; they allocate one for you.
      */
    def `matmulInPlace!`(b: Matrix[Double], c: Matrix[Double], alpha: Double = 1.0, beta: Double = 0.0): Unit =
      dimMatCheck(m, b)
      matmulOutputCheck(m, b, c)
      println("PERFORMING WARNING in matmul on JS")
      println("THIS method copies into native JS types. Then copies back out. Expect catastrophic performance.")

      if m.hasSimpleContiguousMemoryLayout && b.hasSimpleContiguousMemoryLayout then
        val lda = if m.isDenseColMajor then m.rows else m.cols
        val ldb = if b.isDenseColMajor then b.rows else b.cols

        val transB = if b.isDenseColMajor then "no-transpose" else "transpose"
        val transA = if m.isDenseColMajor then "no-transpose" else "transpose"

        // `order` is always "column-major" here, deliberately, even when both operands are dense row-major:
        // `transA`/`transB`/`lda`/`ldb` above are the standard "always column-major" transpose trick (checked
        // against @stdlib/blas's own dgemm source — order alone selects the (stride1, stride2) pair, independent
        // of trans; trans then says whether to read that pair as (row, col) or swap them). Switching `order` to
        // "row-major" here without also inverting `transA`/`transB` would ask dgemm to apply the transpose trick
        // *and* reinterpret the raw strides as row-major, i.e. transpose twice.
        val outArr = new Float64Array(c.raw.toJSArray)
        dgemm(
          "column-major",
          transA,
          transB,
          m.rows,
          b.cols,
          m.cols,
          alpha,
          new Float64Array(m.raw.toJSArray),
          lda,
          new Float64Array(b.raw.toJSArray),
          ldb,
          beta,
          outArr,
          m.rows
        )
        // copy result back into c.raw (Scala Array[Double]) element-wise
        val copyLen = Math.min(outArr.length, c.raw.length)
        var ci = 0
        while ci < copyLen do
          c.raw(ci) = outArr(ci)
          ci += 1
        end while
      else if (m.rowStride == 1 || m.colStride == 1) && (b.rowStride == 1 || b.colStride == 1) then
        val transB = if b.rowStride == 1 then "no-transpose" else "transpose"
        val transA = if m.rowStride == 1 then "no-transpose" else "transpose"

        // See the fully-dense branch above: `order` stays "column-major" regardless of m/b's own orientation,
        // since transA/transB/lda/ldb already implement the transpose trick for that fixed order.
        val outArr = new Float64Array(c.raw.toJSArray)
        dgemm(
          "column-major",
          transA,
          transB,
          m.rows,
          b.cols,
          m.cols,
          alpha,
          // convert backing Scala Array[Double] to Float64Array slice (copies)
          new Float64Array(m.raw.toJSArray).subarray(m.offset),
          if m.rowStride == 1 then m.colStride else m.rowStride,
          new Float64Array(b.raw.toJSArray).subarray(b.offset),
          if b.rowStride == 1 then b.colStride else b.rowStride,
          beta,
          outArr,
          m.rows
        )
        // copy result back into c.raw (Scala Array[Double]) element-wise
        val copyLen2 = Math.min(outArr.length, c.raw.length)
        var cj = 0
        while cj < copyLen2 do
          c.raw(cj) = outArr(cj)
          cj += 1
        end while
      else
        throw UnsupportedLayoutException(
          s"matmulInPlace! does not support this combination of matrix layouts. m: ${m.layoutString}, b: ${b.layoutString}"
        )
      end if

    end `matmulInPlace!`

    /** Writes `alpha * (m @@ vec) + beta * y` into `y` in place, via the stdlib `dgemv` shim. JS counterpart of
      * `JvmDoubleMatrix.*=`; see there for the reasoning.
      *
      * Like CBLAS on Native, this shim takes an `ord` argument, so a `colStride == 1` layout is described directly as
      * `"row-major"` with `lda = rowStride` and a `rowStride == 1` one as `"column-major"` with `lda = colStride`, both
      * keeping `"no-transpose"` and the natural `(rows, cols)`.
      *
      * `lda` was previously `m.rows` for both orders, which is only correct for column-major — under `"row-major"` it
      * is the distance between successive rows and must be at least `cols`, so a non-square dense row-major matrix was
      * read with the wrong stride. It now comes from the layout.
      *
      * Offset views deliberately take the elementwise branch rather than the shim: this facade has no offset parameter,
      * and slicing to fake one would mean yet another copy. That costs nothing real — every call through the shim
      * already marshals the whole backing array into a `Float64Array` and the result back out, so for a product that is
      * `O(rows * cols)` of arithmetic the copies dominate, and the elementwise loop is very likely the faster path on
      * this platform regardless. It is kept because it is the shape the other platforms use.
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
      val asColMajor = nonEmpty && m.offset == 0 && m.rowStride == 1 && m.colStride >= m.rows
      val asRowMajor = nonEmpty && m.offset == 0 && m.colStride == 1 && m.rowStride >= m.cols

      if asColMajor || asRowMajor then
        val yBuf = new Float64Array(y.toJSArray)
        dgemv(
          if asColMajor then "column-major" else "row-major",
          "no-transpose",
          m.rows,
          m.cols,
          alpha,
          new Float64Array(m.raw.toJSArray),
          if asColMajor then m.colStride else m.rowStride,
          new Float64Array(vec.toJSArray),
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

end JsDoubleMatrix

object JvmDoubleMatrix:

end JvmDoubleMatrix

object NativeDoubleMatrix:

end NativeDoubleMatrix

object JvmNativeDoubleMatrix:

end JvmNativeDoubleMatrix
