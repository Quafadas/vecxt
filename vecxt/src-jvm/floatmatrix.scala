package vecxt

import scala.reflect.ClassTag

import vecxt.all.*
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
      else if (m.rowStride == 1 || m.colStride == 1) && (b.rowStride == 1 || b.colStride == 1) then
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

    @targetName("matmulFloatVector")
    def *(vec: Array[Float], alpha: Float, beta: Float): Array[Float] =

      if m.isDenseColMajor then
        require(vec.length == m.cols, s"Vector length ${vec.length} != expected ${m.cols}")
        val newArr = Array.ofDim[Float](m.rows)
        val out = Array.fill(m.rows)(0.0)

        blas.sgemv(
          "N",
          m.rows,
          m.cols,
          alpha,
          m.raw,
          m.rows,
          vec,
          1,
          beta,
          newArr,
          1
        )

        newArr
      else ???
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

    private inline def reduceAlongDimension(
        dim: DimensionExtender,
        inline op: (Float, Float) => Float,
        inline initial: Float
    ): Matrix[Float] =
      if !m.hasSimpleContiguousMemoryLayout then ???
      end if
      val whichDim = dim.asInt
      val newShape = m.shape match
        case (r, c) if whichDim == 0 => (r, 1)
        case (r, c) if whichDim == 1 => (1, c)
        case _                       => ???

      val newArr = Array.fill(newShape._1 * newShape._2)(initial)
      var i = 0
      while i < m.cols do
        var j = 0
        while j < m.rows do
          val idx = i * m.rows + j
          if whichDim == 0 then newArr(j) = op(newArr(j), m.raw(idx))
          end if
          if whichDim == 1 then newArr(i) = op(newArr(i), m.raw(idx))
          end if
          j += 1
        end while
        i += 1
      end while

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

object NativeFloatMatrix:

end NativeFloatMatrix
