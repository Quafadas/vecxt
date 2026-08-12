package vecxt

import scala.reflect.ClassTag

import vecxt.all.*
import vecxt.annotations.AllocFree
import vecxt.dimensionExtender.DimensionExtender.*
import dev.ludovic.netlib.blas.JavaBLAS.getInstance as blas
import jdk.incubator.vector.*
import scala.annotation.targetName

object JvmFloatMatrix:

  private final val spf = FloatVector.SPECIES_PREFERRED
  private final val spfl = spf.length()

  private final val sp_int_floatLanes =
    VectorSpecies.of(java.lang.Integer.TYPE, VectorShape.forBitSize(spfl * Integer.SIZE));

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

    /** Writes `alpha * (m @@ b) + beta * c` into `c` in place, via BLAS `sgemm`.
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

      val lda = if m.isDenseColMajor then m.rows else m.cols
      val ldb = if b.isDenseColMajor then b.rows else b.cols

      if m.hasSimpleContiguousMemoryLayout && b.hasSimpleContiguousMemoryLayout then
        val mStr = if m.isDenseColMajor then "N" else "T"
        val bStr = if b.isDenseColMajor then "N" else "T"
        blas.sgemm(
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
      else if blasLeadingDimensionCheck(m) && blasLeadingDimensionCheck(b) then
        val mStr = if m.rowStride == 1 then "N" else "T"
        val bStr = if b.rowStride == 1 then "N" else "T"
        // If the matrix has an offset, then a call to blas.sgemm complains.
        // https://github.com/luhenry/netlib/issues/23
        blas.sgemm(
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

    @targetName("matmulFloatElementWise")
    def *:*(bmat: Matrix[Boolean]): Matrix[Float] =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        // Preserve m's own orientation: deepCopy defaults to column-major, which would mismatch bmat's
        // orientation whenever m (and hence bmat, per the check above) is dense row-major, sending the
        // `*:*=` below into its slower foreach2D fallback instead of the fast path both were just confirmed to share.
        val copy = m.deepCopy(asRowMajor = m.isDenseRowMajor)
        copy *:*= bmat
        copy
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val bIdx = bmat.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = if bmat.raw(bIdx) then m.raw(mIdx) else 0.0f
        }
        Matrix[Float](newArr, m.rows, m.cols)
      end if
    end *:*

    @targetName("matmulFloatElementWiseInPlace")
    def *:*=(bmat: Matrix[Boolean]): Unit =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        val zero = FloatVector.zero(spf)
        var i = 0
        while i < spf.loopBound(m.raw.length) do
          val mask = VectorMask.fromArray(spf, bmat.raw, i)
          // keep float value where mask=true, zero where mask=false
          zero.blend(FloatVector.fromArray(spf, m.raw, i), mask).intoArray(m.raw, i)
          i += spfl
        end while
        while i < m.raw.length do
          if !bmat.raw(i) then m.raw.update(i, 0.0f)
          end if
          i += 1
        end while
      else
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val bIdx = bmat.layout.linearIndex(i, j)
          if !bmat.raw(bIdx) then m.raw(mIdx) = 0.0f
          end if
        }
      end if
    end *:*=

    // inline def @@(b: Matrix[Double]): Matrix[Double] = m.matmul(b)

    // inline def *:*=(d: Double): Unit = m.raw.multInPlace(d)

    // TODO: Dim check

    /** Writes `alpha * (m @@ vec) + beta * y` into `y` in place, via BLAS `sgemv`. Float counterpart of
      * `JvmDoubleMatrix.*=`, identical in every respect but element type — see there for why `TRANS`/`lda` are chosen
      * from the strides, why the `stride >= extent` half of each guard is load-bearing, and why the elementwise
      * fallback branches on `beta == 0` explicitly.
      *
      * `alpha` and `beta` have no defaults, unlike the `Double` twin. `all` exports both element types into one scope,
      * and Scala permits only one overload of a name to carry default arguments — the same reason `matmulInPlace!`
      * already spells its `Float` arguments out while the `Double` one defaults them. The choice of which side keeps
      * the defaults is arbitrary; keeping it where it already was is not.
      *
      * @param vec
      *   the vector to multiply by; must have length `m.cols`
      * @param y
      *   the destination, accumulated onto per `beta`; must have length `m.rows`
      */
    @AllocFree
    def *=(vec: Array[Float], y: Array[Float], alpha: Float, beta: Float): Unit =
      if vec.length != m.cols then
        throw new IllegalArgumentException(s"Vector length ${vec.length} != expected ${m.cols}")
      end if
      if y.length != m.rows then
        throw new IllegalArgumentException(s"Destination length ${y.length} != expected ${m.rows}")
      end if
      val nonEmpty = m.rows > 0 && m.cols > 0

      if nonEmpty && m.rowStride == 1 && m.colStride >= m.rows then
        blas.sgemv("N", m.rows, m.cols, alpha, m.raw, m.offset, m.colStride, vec, 0, 1, beta, y, 0, 1)
      else if nonEmpty && m.colStride == 1 && m.rowStride >= m.cols then
        blas.sgemv("T", m.cols, m.rows, alpha, m.raw, m.offset, m.rowStride, vec, 0, 1, beta, y, 0, 1)
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

    /** Matrix-vector product: returns `m @@ vec` as a fresh array. Wrapper over [[*=]] with `beta = 0`, so the freshly
      * allocated destination is written without being read. See `JvmDoubleMatrix.*` for why there is no `beta`
      * parameter.
      *
      * Two arities rather than a defaulted `alpha`, for the reason given on [[*=]]: the `Double` overload of `*`
      * already carries the one set of default arguments this name is allowed across both element types.
      */
    @targetName("matmulFloatVector")
    def *(vec: Array[Float]): Array[Float] = m.*(vec, 1.0f)

    @targetName("matmulFloatVectorScaled")
    def *(vec: Array[Float], alpha: Float): Array[Float] =
      val out = Array.ofDim[Float](m.rows)
      m.*=(vec, out, alpha, 0.0f)
      out
    end *

    def >=(d: Float): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](vecxt.floatarrays.>=(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) >= d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    @targetName("floatmatrixGT")
    def >(d: Float): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](vecxt.floatarrays.>(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) > d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    @targetName("floatmatrixLE")
    def <=(d: Float): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](vecxt.floatarrays.<=(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) <= d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    @targetName("floatmatrixLT")
    def <(d: Float): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](vecxt.floatarrays.<(m.raw)(d), m.layout)
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
    @targetName("floatmatrixAddVectorInPlace")
    def +=(arr: Array[Float]): Unit =

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
          while j < spf.loopBound(m.rows) do

            val offsetJ = offsetI + j
            FloatVector
              .fromArray(
                spf,
                m.raw,
                offsetJ
              )
              .add(
                FloatVector.broadcast(spf, arr(i))
              )
              .intoArray(
                m.raw,
                offsetJ
              )

            j += spf.length()

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
          while i < spf.loopBound(m.cols) do
            val offsetI = offsetJ + i
            FloatVector
              .fromArray(
                spf,
                m.raw,
                offsetI
              )
              .add(
                FloatVector.fromArray(spf, arr, i)
              )
              .intoArray(
                m.raw,
                offsetI
              )
            i += spf.length()

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

    @targetName("floatmatrixSubVector")
    def -(mat1: Matrix[Float]): Matrix[Float] =
      sameDimMatCheck(m, mat1)
      if sameDenseElementWiseMemoryLayoutCheck(m, mat1) then
        val newArr = vecxt.floatarrays.-(m.raw)(mat1.raw)
        Matrix(newArr, m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        val newMat =
          Matrix[Float](newArr, m.rows, m.cols, m.cols, 1, 0)
        m.layout.foreach2D { (i, j) =>
          newMat(i, j) = m(i, j) - mat1(i, j)
        }
        newMat
      end if
    end -

    @targetName("floatmatrixSubVectorInPlace")
    def -=(arr: Array[Float]): Unit =

      assert(arr.length == m.cols, s"Array length ${arr.length} != expected ${m.cols}")

      if m.rowStride == 1 then
        var i = 0
        while i < m.cols do

          var j = 0
          val offsetI = m.offset + i * m.colStride
          while j < spf.loopBound(m.rows) do

            val offsetJ = offsetI + j
            FloatVector
              .fromArray(
                spf,
                m.raw,
                offsetJ
              )
              .sub(
                FloatVector.broadcast(spf, arr(i))
              )
              .intoArray(
                m.raw,
                offsetJ
              )

            j += spf.length()

          end while
          while j < m.rows do
            val idx = offsetI + j
            m.raw(idx) = m.raw(idx) - arr(i)
            j += 1
          end while

          i += 1
        end while
      else if m.colStride == 1 then
        var j = 0
        while j < m.rows do
          var i = 0
          val offsetJ = m.offset + j * m.rowStride
          while i < spf.loopBound(m.cols) do
            val offsetI = offsetJ + i
            FloatVector
              .fromArray(
                spf,
                m.raw,
                offsetI
              )
              .sub(
                FloatVector.fromArray(spf, arr, i)
              )
              .intoArray(
                m.raw,
                offsetI
              )
            i += spf.length()

          end while
          while i < m.cols do
            val idx = offsetJ + i
            m.raw(idx) = m.raw(idx) - arr(i)
            i += 1
          end while
          j = j + 1
        end while
      else
        m.layout.foreach2D { (i, j) =>
          m(i, j) = m(i, j) - arr(j)
        }
      end if

    end -=

    @targetName("floatmatrixAddScalarInPlace")
    def +=(n: Float): Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.floatarrays.+=(m.raw)(n)
      else
        // println(s" .offset: ${m.offset}, m.rowStride: ${m.rowStride}, m.colStride: ${m.colStride}")
        // Cache-friendly fallback: iterate with smallest stride in inner loop
        // (m.offset + row * m.rowStride + col * m.colStride
        if m.rowStride <= m.colStride then
          // Row stride is smaller, so iterate rows in inner loop
          val rowStrides = IntVector.zero(sp_int_floatLanes).addIndex(m.rowStride).toArray
          // println(m.offset)
          // println(s"colStrides: ${rowStrides.mkString(", ")}")
          // println(s"m.raw: ${m.raw.mkString(", ")}")
          // println(s"m.rows: ${m.rows}, m.cols: ${m.cols}")
          // println(s"m.rowStride: ${m.rowStride}, m.colStride: ${m.colStride}")
          var j = 0
          while j < m.cols do
            var i = 0
            var blockIndex = m.offset + j * m.colStride
            val upperBound = sp_int_floatLanes.loopBound(m.rows)
            while i < upperBound do
              val iBlockIndex = blockIndex + i * m.rowStride
              FloatVector
                .fromArray(spf, m.raw, iBlockIndex, rowStrides, 0)
                .add(n)
                .intoArray(m.raw, iBlockIndex, rowStrides, 0)
              i += sp_int_floatLanes.length()
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
          val colStrides = IntVector.zero(sp_int_floatLanes).addIndex(m.colStride).toArray
          // println(m.offset)
          // println(s"colStrides: ${colStrides.mkString(", ")}")
          // println(s"m.raw: ${m.raw.mkString(", ")}")
          // println(s"m.rows: ${m.rows}, m.cols: ${m.cols}")
          // println(s"m.rowStride: ${m.rowStride}, m.colStride: ${m.colStride}")
          var i = 0
          while i < m.rows do
            var j = 0
            val upperBound = sp_int_floatLanes.loopBound(m.cols)

            var blockIndex = m.offset + i * m.rowStride
            while j < upperBound do
              val jblockIndex = blockIndex + j * m.colStride
              FloatVector
                .fromArray(spf, m.raw, jblockIndex, colStrides, 0)
                .add(n)
                .intoArray(m.raw, jblockIndex, colStrides, 0)

              j += sp_int_floatLanes.length()
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

    @targetName("floatmatrixSubScalarInPlace")
    def -=(n: Float): Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.floatarrays.-=(m.raw)(n)
      else
        if m.rowStride <= m.colStride then
          val rowStrides = IntVector.zero(sp_int_floatLanes).addIndex(m.rowStride).toArray
          var j = 0
          while j < m.cols do
            var i = 0
            var blockIndex = m.offset + j * m.colStride
            val upperBound = sp_int_floatLanes.loopBound(m.rows)
            while i < upperBound do
              val iBlockIndex = blockIndex + i * m.rowStride
              FloatVector
                .fromArray(spf, m.raw, iBlockIndex, rowStrides, 0)
                .sub(n)
                .intoArray(m.raw, iBlockIndex, rowStrides, 0)
              i += sp_int_floatLanes.length()
            end while
            while i < m.rows do
              m.elementIndex(i, j)
              m(i, j) = m(i, j) - n
              i += 1
            end while

            j += 1
          end while
        else
          val colStrides = IntVector.zero(sp_int_floatLanes).addIndex(m.colStride).toArray
          var i = 0
          while i < m.rows do
            var j = 0
            val upperBound = sp_int_floatLanes.loopBound(m.cols)

            var blockIndex = m.offset + i * m.rowStride
            while j < upperBound do
              val jblockIndex = blockIndex + j * m.colStride
              FloatVector
                .fromArray(spf, m.raw, jblockIndex, colStrides, 0)
                .sub(n)
                .intoArray(m.raw, jblockIndex, colStrides, 0)

              j += sp_int_floatLanes.length()
            end while

            while j < m.cols do
              m.elementIndex(i, j)
              m(i, j) = m(i, j) - n
              j += 1
            end while
            i += 1
          end while
        end if
      end if

    end -=

    def *=(d: Float): Unit =
      if m.hasSimpleContiguousMemoryLayout then floatarrays.*=(m.raw)(d)
      else ???
    end *=

    def *(d: Float): Matrix[Float] =
      val out = m.deepCopy
      out.*=(d)
      out

    end *

    def +(d: Float): Matrix[Float] =
      val out = m.deepCopy
      out.+=(d)
      out
    end +

    /** Returns the sum of each column as a flat Array[Float].
      *
      * For dense column-major matrices the SIMD reduction runs directly on the backing array at each column's base
      * offset — no intermediate array is allocated per column.
      */
    def colSums: Array[Float] =
      val result = Array.ofDim[Float](m.cols)
      var i = 0
      if m.isDenseColMajor then
        while i < m.cols do
          val colBase = i * m.rows
          var j = 0
          var acc = FloatVector.zero(spf)
          while j < spf.loopBound(m.rows) do
            acc = acc.add(FloatVector.fromArray(spf, m.raw, colBase + j))
            j += spfl
          end while
          var temp = acc.reduceLanes(VectorOperators.ADD)
          while j < m.rows do
            temp += m.raw(colBase + j)
            j += 1
          end while
          result(i) = temp
          i += 1
        end while
      else
        while i < m.cols do
          var acc = 0.0f
          var j = 0
          while j < m.rows do
            acc += m((j, i))
            j += 1
          end while
          result(i) = acc
          i += 1
        end while
      end if
      result
    end colSums

    /** Reads every element through `m.layout.linearIndex`, which is just `offset + row * rowStride + col * colStride` —
      * valid for any layout, dense or strided, row-major or column-major. So unlike the element-wise SIMD ops in this
      * file (which need a simple contiguous layout to hand `m.raw` to a vectorized loop), this needs no
      * `hasSimpleContiguousMemoryLayout` guard: there's no fast path to fall back from, just this one loop. `foreach2D`
      * picks whichever of row-major/column-major traversal order is cache-friendly for `m`'s own layout; which order it
      * picks doesn't matter for correctness here since `op` (max/min/sum/product) is always commutative and
      * associative.
      */
    private inline def reduceAlongDimension(
        dim: DimensionExtender,
        inline op: (Float, Float) => Float,
        inline initial: Float
    ): Matrix[Float] =
      val whichDim = dim.asInt
      val newShape = m.shape match
        case (r, c) if whichDim == 0 => (r, 1)
        case (r, c) if whichDim == 1 => (1, c)
        case _                       => throw InvalidDimensionException(whichDim)

      val newArr = Array.fill(newShape._1 * newShape._2)(initial)
      m.layout.foreach2D { (i, j) =>
        val idx = m.layout.linearIndex(i, j)
        if whichDim == 0 then newArr(i) = op(newArr(i), m.raw(idx))
        end if
        if whichDim == 1 then newArr(j) = op(newArr(j), m.raw(idx))
        end if
      }

      Matrix[Float](newArr, newShape)
    end reduceAlongDimension

    @targetName("floatMatrixMax")
    def max(dim: DimensionExtender): Matrix[Float] =
      reduceAlongDimension(dim, math.max, Float.MinValue)
    end max

    @targetName("floatMatrixMin")
    def min(dim: DimensionExtender): Matrix[Float] =
      reduceAlongDimension(dim, math.min, Float.MaxValue)
    end min

    @targetName("floatMatrixSum")
    def sum(dim: DimensionExtender): Matrix[Float] =
      reduceAlongDimension(dim, _ + _, 0.0f)
    end sum

    @targetName("floatMatrixProduct")
    def product(dim: DimensionExtender): Matrix[Float] =
      reduceAlongDimension(dim, _ * _, 1.0f)
    end product

  end extension

  extension (d: Float)
    def *(m: Matrix[Float]): Matrix[Float] = m * d

    def +(m: Matrix[Float]): Matrix[Float] = m + d

    def -(m: Matrix[Float]): Matrix[Float] = ???
    def /(m: Matrix[Float]): Matrix[Float] = ???

    def *=(m: Matrix[Float]): Unit = m *= d
    def +=(m: Matrix[Float]): Unit = ??? // m += d
    def -=(m: Matrix[Float]): Unit = ??? // m -= d
    def /=(m: Matrix[Float]): Unit = ???

  end extension

end JvmFloatMatrix

/** Cross-compilation stubs — the real implementations live in `src-js/floatmatrix.scala` and
  * `src-native/floatmatrix.scala`. `all` exports both names on every platform, so both have to exist here too. Same
  * arrangement `JvmDoubleMatrix`/`JsDoubleMatrix`/`NativeDoubleMatrix` already use. `NativeFloatMatrix` was already
  * declared empty here before either had an implementation.
  */
object NativeFloatMatrix:

end NativeFloatMatrix

object JsFloatMatrix
