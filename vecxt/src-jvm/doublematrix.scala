package vecxt

import scala.reflect.ClassTag

import vecxt.all.*

import dev.ludovic.netlib.blas.JavaBLAS.getInstance as blas
import jdk.incubator.vector.*

object JvmDoubleMatrix:

  final val sp_int_doubleLanes =
    VectorSpecies.of(java.lang.Integer.TYPE, VectorShape.forBitSize(vecxt.doublearrays.spdl * Integer.SIZE));

  extension (m: Matrix[Double]) // inline def /(n: Double): Matrix[Double] =
    //   Matrix(vecxt.arrays./(m.raw)(n), m.shape)

    /** Writes `alpha * (m @@ b) + beta * c` into `c` in place, via BLAS `dgemm`.
      *
      * `c` must already be shaped `(m.rows, b.cols)` and dense column-major — `ldc` is hardcoded to `m.rows` below, and
      * `dgemm` also reads `c` when `beta != 0`, so any other shape or layout would be silently written to (or read
      * from) incorrectly rather than rejected. Use `matmul`/`@@` instead if you don't already have a conforming `c` to
      * write into; they allocate one for you.
      */
    def `matmulInPlace!`(b: Matrix[Double], c: Matrix[Double], alpha: Double = 1.0, beta: Double = 0.0): Unit =
      dimMatCheck(m, b)
      matmulOutputCheck(m, b, c)

      val lda = if m.isDenseColMajor then m.rows else m.cols
      val ldb = if b.isDenseColMajor then b.rows else b.cols

      if m.hasSimpleContiguousMemoryLayout && b.hasSimpleContiguousMemoryLayout then
        val mStr = if m.isDenseColMajor then "N" else "T"
        val bStr = if b.isDenseColMajor then "N" else "T"
        blas.dgemm(
          mStr,
          bStr,
          m.rows,
          b.cols,
          m.cols,
          alpha,
          m.raw,
          0,
          lda,
          b.raw,
          0,
          ldb,
          beta,
          c.raw,
          0,
          m.rows
        )
      else if (m.rowStride == 1 || m.colStride == 1) && (b.rowStride == 1 || b.colStride == 1) then
        val mStr = if m.rowStride == 1 then "N" else "T"
        val bStr = if b.rowStride == 1 then "N" else "T"
        // If the matrix has an offset, then a call to blas.dgemm complains.
        // https://github.com/luhenry/netlib/issues/23
        blas.dgemm(
          mStr,
          bStr,
          m.rows,
          b.cols,
          m.cols,
          alpha,
          m.raw,
          m.offset,
          if m.rowStride == 1 then m.colStride else m.rowStride,
          b.raw,
          b.offset,
          // Checks b.rowStride (not b.colStride) to match bStr above and the equivalent JS/Native expressions:
          // numerically identical to `if b.colStride == 1 then b.rowStride else b.colStride` once exactly one of
          // b's strides is 1 (guaranteed by the guard above), but written so a future edit to one platform's
          // condition doesn't silently diverge from the others.
          if b.rowStride == 1 then b.colStride else b.rowStride,
          beta,
          c.raw,
          c.offset,
          m.rows
        )
      else
        throw UnsupportedLayoutException(
          s"matmulInPlace! does not support this combination of matrix layouts. m: ${m.layoutString}, b: ${b.layoutString}"
        )
      end if

    end `matmulInPlace!`

    def *:*(bmat: Matrix[Boolean]): Matrix[Double] =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        // Preserve m's own orientation: deepCopy defaults to column-major, which would mismatch bmat's
        // orientation whenever m (and hence bmat, per the check above) is dense row-major, sending the
        // `*:*=` below into its slower foreach2D fallback instead of the fast path both were just confirmed to share.
        val copy = m.deepCopy(asRowMajor = m.isDenseRowMajor)
        copy *:*= bmat
        copy
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
        val spd = doublearrays.spd
        val spdl = doublearrays.spdl
        val zero = DoubleVector.zero(spd)
        var i = 0
        while i < spd.loopBound(m.raw.length) do
          val mask = VectorMask.fromArray(spd, bmat.raw, i)
          zero.blend(DoubleVector.fromArray(spd, m.raw, i), mask).intoArray(m.raw, i)
          i += spdl
        end while
        while i < m.raw.length do
          if !bmat.raw(i) then m.raw.update(i, 0.0)
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

    // inline def @@(b: Matrix[Double]): Matrix[Double] = m.matmul(b)

    // inline def *:*=(d: Double): Unit = m.raw.multInPlace(d)

    // TODO: Dim check

    /** Matrix-vector product: returns `alpha * (m @@ vec)` as a fresh `Array[Double]` of length `m.rows`.
      *
      * `dgemv` addresses `A` as one column-major block described by a single leading dimension — `A(p, q)` lives at
      * `a(offset + p + q * lda)`. A layout is therefore expressible exactly when one of its strides is `1` and the
      * other is a usable leading dimension, and the same `TRANS` trick `matmulInPlace!` already uses for `dgemm`
      * covers both orientations rather than only column-major:
      *
      *   - `rowStride == 1` — the array already is `A` in the form `dgemv` wants, so `TRANS = "N"`, `lda = colStride`,
      *     and the dimensions pass straight through as `(rows, cols)`.
      *   - `colStride == 1` — reading `(p, q)` as `(col, row)` instead makes the very same memory a column-major
      *     `cols x rows` block holding `Aᵀ`, so `TRANS = "T"`, `lda = rowStride`, and the dimensions are handed over
      *     swapped as `(cols, rows)`. `dgemv` then transposes it back and computes `A * vec` as asked.
      *
      * Passing `m.offset` covers submatrix views in both cases, exactly as `matmulInPlace!` does.
      *
      * The `stride >= extent` half of each guard is not decoration. `lda` must be at least the block's row count or
      * BLAS rejects the call, and layouts exist that satisfy `stride == 1` while failing it: a broadcast column
      * (`colStride == 0`) repeats one column across the matrix, and no leading dimension expresses that. Broadcasts,
      * negative strides and doubly-strided views therefore fall to the elementwise loop, which reads through
      * `linearIndex` and is correct for any layout at all. An empty matrix is routed there too, to keep a degenerate
      * shape away from BLAS.
      *
      * `beta` is inert and kept only for source compatibility: `dgemv` computes `y := alpha*A*x + beta*y`, but `y`
      * here is `newArr`, freshly allocated and therefore all zeroes, so `beta * y` contributes nothing for any finite
      * `beta`. It would only mean something on an API that accepted the destination from the caller, as
      * `matmulInPlace!` does.
      *
      * @param vec
      *   the vector to multiply by; must have length `m.cols`
      * @return
      *   a new array of length `m.rows`
      */
    def *(vec: Array[Double], alpha: Double = 1.0, beta: Double = 1.0): Array[Double] =
      require(vec.length == m.cols, s"Vector length ${vec.length} != expected ${m.cols}")
      val newArr = Array.ofDim[Double](m.rows)
      val nonEmpty = m.rows > 0 && m.cols > 0

      if nonEmpty && m.rowStride == 1 && m.colStride >= m.rows then
        blas.dgemv("N", m.rows, m.cols, alpha, m.raw, m.offset, m.colStride, vec, 0, 1, beta, newArr, 0, 1)
      else if nonEmpty && m.colStride == 1 && m.rowStride >= m.cols then
        blas.dgemv("T", m.cols, m.rows, alpha, m.raw, m.offset, m.rowStride, vec, 0, 1, beta, newArr, 0, 1)
      else
        var i = 0
        while i < m.rows do
          var acc = 0.0
          var j = 0
          while j < m.cols do
            acc += m.raw(m.layout.linearIndex(i, j)) * vec(j)
            j += 1
          end while
          newArr(i) = alpha * acc
          i += 1
        end while
      end if

      newArr
    end *

    def >=(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](vecxt.doublearrays.>=(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) >= d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    def >(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](vecxt.doublearrays.>(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) > d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    def <=(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](vecxt.doublearrays.<=(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) <= d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    def <(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](vecxt.doublearrays.<(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) < d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    /** Adds the elements of this vector to the matrix with broadcasting behavior.
      *
      * Depending on the matrix's memory layout:
      *   1. If `rowStride == 1`, the elements of the vector are broadcasted down each column and added.
      *   2. If `colStride == 1`, the elements of the vector are added to each row directly.
      *   3. Otherwise, a fallback mechanism is used.
      *
      * @param arr
      *   The vector to be added to the matrix.
      * @param boundsCheck
      *   Whether to perform bounds checking on the vector length.
      */
    def +=(arr: Array[Double]): Unit =

      assert(arr.length == m.cols, s"Array length ${arr.length} != expected ${m.cols}")

      /**   1. If rowStride = 1, then we can broadcast each element of arr down each column SIMD
        *   2. If colStride = 1, then we can add each element of the vector to each row
        *
        * else fallback
        */

      if m.rowStride == 1 then
        var i = 0
        while i < m.cols do

          var j = 0
          val offsetI = m.offset + i * m.colStride
          while j < spd.loopBound(m.rows) do

            val offsetJ = offsetI + j
            DoubleVector
              .fromArray(
                vecxt.doublearrays.spd,
                m.raw,
                offsetJ
              )
              .add(
                DoubleVector.broadcast(vecxt.doublearrays.spd, arr(i))
              )
              .intoArray(
                m.raw,
                offsetJ
              )

            j += spd.length

          end while
          while j < m.rows do
            val idx = offsetI + j
            m.raw(idx) = m.raw(idx) + arr(i)
            j += 1
          end while

          i += 1
        end while
      else if m.colStride == 1 then
        var j = 0
        while j < m.rows do
          var i = 0
          val offsetJ = m.offset + j * m.rowStride
          while i < spd.loopBound(m.cols) do
            val offsetI = offsetJ + i
            DoubleVector
              .fromArray(
                vecxt.doublearrays.spd,
                m.raw,
                offsetI
              )
              .add(
                DoubleVector.fromArray(vecxt.doublearrays.spd, arr, i)
              )
              .intoArray(
                m.raw,
                offsetI
              )
            i += spd.length()

          end while
          while i < m.cols do
            val idx = offsetJ + i
            m.raw(idx) = m.raw(idx) + arr(i)
            i += 1
          end while
          j = j + 1
        end while
      else // fallback for strides != 1
        m.layout.foreach2D { (i, j) =>
          m(i, j) = m(i, j) + arr(j)
        }
      end if

    end +=

    def +=(n: Double): Unit =

      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.+=(m.raw)(n)
      else
        // println(s" .offset: ${m.offset}, m.rowStride: ${m.rowStride}, m.colStride: ${m.colStride}")
        // Cache-friendly fallback: iterate with smallest stride in inner loop
        // (m.offset + row * m.rowStride + col * m.colStride
        if m.rowStride <= m.colStride then
          // Row stride is smaller, so iterate rows in inner loop
          val rowStrides = IntVector.zero(sp_int_doubleLanes).addIndex(m.rowStride).toArray
          // println(m.offset)
          // println(s"colStrides: ${rowStrides.mkString(", ")}")
          // println(s"m.raw: ${m.raw.mkString(", ")}")
          // println(s"m.rows: ${m.rows}, m.cols: ${m.cols}")
          // println(s"m.rowStride: ${m.rowStride}, m.colStride: ${m.colStride}")
          var j = 0
          while j < m.cols do
            var i = 0
            var blockIndex = m.offset + j * m.colStride
            val upperBound = sp_int_doubleLanes.loopBound(m.rows)
            while i < upperBound do
              val iBlockIndex = blockIndex + i * m.rowStride
              DoubleVector
                .fromArray(vecxt.doublearrays.spd, m.raw, iBlockIndex, rowStrides, 0)
                .add(n)
                .intoArray(m.raw, iBlockIndex, rowStrides, 0)
              i += sp_int_doubleLanes.length
            end while
            while i < m.rows do
              m.elementIndex(i, j)
              m(i, j) = n + m(i, j)
              i += 1
            end while

            j += 1
          end while
        else
          // Column stride is smaller, so iterate columns in inner loop
          val colStrides = IntVector.zero(sp_int_doubleLanes).addIndex(m.colStride).toArray
          // println(m.offset)
          // println(s"colStrides: ${colStrides.mkString(", ")}")
          // println(s"m.raw: ${m.raw.mkString(", ")}")
          // println(s"m.rows: ${m.rows}, m.cols: ${m.cols}")
          // println(s"m.rowStride: ${m.rowStride}, m.colStride: ${m.colStride}")
          var i = 0
          while i < m.rows do
            var j = 0
            val upperBound = sp_int_doubleLanes.loopBound(m.cols)

            var blockIndex = m.offset + i * m.rowStride
            while j < upperBound do
              val jblockIndex = blockIndex + j * m.colStride
              DoubleVector
                .fromArray(vecxt.doublearrays.spd, m.raw, jblockIndex, colStrides, 0)
                .add(n)
                .intoArray(m.raw, jblockIndex, colStrides, 0)

              j += sp_int_doubleLanes.length
            end while

            while j < m.cols do
              m.elementIndex(i, j)
              m(i, j) = n + m(i, j)
              j += 1
            end while
            i += 1
          end while
        end if
      end if

    end +=

  end extension

end JvmDoubleMatrix

object NativeDoubleMatrix:

end NativeDoubleMatrix
