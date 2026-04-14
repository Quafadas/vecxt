package vecxt

import scala.reflect.ClassTag

import vecxt.BoundsCheck.BoundsCheck
import vecxt.all.*
import scala.util.chaining.*
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
    inline def @@(b: Matrix[Float])(using inline boundsCheck: BoundsCheck): Matrix[Float] =
      m.matmul(b, 1.0f, 0.0f)

    @targetName("matmulFloatNonDefault")
    inline def matmul(b: Matrix[Float], alpha: Float, beta: Float)(using
        inline boundsCheck: BoundsCheck
    ): Matrix[Float] =
      dimMatCheck(m, b)
      val newArr: Array[Float] = Array.ofDim[Float](m.rows * b.cols)
      val newmat = Matrix[Float](newArr, m.rows, b.cols)
      m.`matmulInPlace!`(b, newmat, alpha, beta)(using boundsCheck)
      newmat
    end matmul

    @targetName("matmulFloatInPlace")
    inline def `matmulInPlace!`(b: Matrix[Float], c: Matrix[Float], alpha: Float, beta: Float)(using
        inline boundsCheck: BoundsCheck
    ): Unit =
      dimMatCheck(m, b)

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
      else if m.rowStride == 1 || m.colStride == 1 && b.rowStride == 1 || b.colStride == 1 then
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
          if b.colStride == 1 then b.rowStride else b.colStride,
          beta,
          c.raw,
          c.offset,
          m.rows
        )
      else ???
      end if

    end `matmulInPlace!`

    @targetName("matmulFloatElementWise")
    inline def *:*(bmat: Matrix[Boolean])(using inline boundsCheck: BoundsCheck): Matrix[Float] =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        val copy = m.deepCopy
        copy *:*= bmat
        copy
      else
        val newArr = Array.ofDim[Float](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val mIdx = m.offset + i * m.rowStride + j * m.colStride
            val bIdx = bmat.offset + i * bmat.rowStride + j * bmat.colStride
            newArr(i + j * m.rows) = if bmat.raw(bIdx) then m.raw(mIdx) else 0.0f
            j += 1
          end while
          i += 1
        end while
        Matrix[Float](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)
      end if
    end *:*

    @targetName("matmulFloatElementWiseInPlace")
    inline def *:*=(bmat: Matrix[Boolean])(using inline boundsCheck: BoundsCheck): Unit =
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
      else ???
      end if
    end *:*=

    // inline def @@(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m.matmul(b)

    // inline def *:*=(d: Double): Unit = m.raw.multInPlace(d)

    // TODO: Dim check

    @targetName("matmulFloatVector")
    inline def *(vec: Array[Float], alpha: Float, beta: Float)(using
        inline boundsCheck: BoundsCheck
    ): Array[Float] =

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

    inline def >=(d: Float): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Boolean](vecxt.floatarrays.>=(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = m.raw(srcIdx) >= d
            j += 1
          end while
          i += 1
        end while
        Matrix[Boolean](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)

    @targetName("floatmatrixGT")
    inline def >(d: Float): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Boolean](vecxt.floatarrays.>(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = m.raw(srcIdx) > d
            j += 1
          end while
          i += 1
        end while
        Matrix[Boolean](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)

    @targetName("floatmatrixLE")
    inline def <=(d: Float): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Boolean](vecxt.floatarrays.<=(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = m.raw(srcIdx) <= d
            j += 1
          end while
          i += 1
        end while
        Matrix[Boolean](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)

    @targetName("floatmatrixLT")
    inline def <(d: Float): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Boolean](vecxt.floatarrays.<(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = m.raw(srcIdx) < d
            j += 1
          end while
          i += 1
        end while
        Matrix[Boolean](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)

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
    inline def +=(arr: Array[Float])(using inline boundsCheck: BoundsCheck): Unit =

      if boundsCheck then assert(arr.length == m.cols, s"Array length ${arr.length} != expected ${m.cols}")
      end if

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
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            m(i, j) = m(i, j) + arr(j)
            j += 1
          end while
          i += 1
        end while
      end if

    end +=

    @targetName("floatmatrixSubVector")
    inline def -(mat1: Matrix[Float])(using inline boundsCheck: BoundsCheck): Matrix[Float] =
      sameDimMatCheck(m, mat1)
      if sameDenseElementWiseMemoryLayoutCheck(m, mat1) then
        val newArr = vecxt.floatarrays.-(m.raw)(mat1.raw)
        Matrix[Float](newArr, m.rows, m.cols, m.rowStride, m.colStride, m.offset)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Float](m.numel)
        val newMat =
          Matrix[Float](newArr, m.rows, m.cols, m.cols, 1, 0)(using BoundsCheck.DoBoundsCheck.no)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            newMat(i, j) = m(i, j) - mat1(i, j)
            j += 1
          end while
          i += 1
        end while
        newMat
      end if
    end -

    @targetName("floatmatrixSubVectorInPlace")
    inline def -=(arr: Array[Float])(using inline boundsCheck: BoundsCheck): Unit =

      if boundsCheck then assert(arr.length == m.cols, s"Array length ${arr.length} != expected ${m.cols}")
      end if

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
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            m(i, j) = m(i, j) - arr(j)
            j += 1
          end while
          i += 1
        end while
      end if

    end -=

    @targetName("floatmatrixAddScalarInPlace")
    def +=(n: Float): Unit =
      import vecxt.BoundsCheck.DoBoundsCheck.no
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
              m.elementIndex(i, j)(using BoundsCheck.DoBoundsCheck.yes)
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
              m.elementIndex(i, j)(using BoundsCheck.DoBoundsCheck.yes)
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
      import vecxt.BoundsCheck.DoBoundsCheck.no
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
              m.elementIndex(i, j)(using BoundsCheck.DoBoundsCheck.yes)
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
              m.elementIndex(i, j)(using BoundsCheck.DoBoundsCheck.yes)
              m(i, j) = m(i, j) - n
              j += 1
            end while
            i += 1
          end while
        end if
      end if

    end -=

    inline def *=(d: Float): Unit =
      if m.hasSimpleContiguousMemoryLayout then floatarrays.*=(m.raw)(d)
      else ???
    end *=

    inline def *(d: Float): Matrix[Float] =
      m.deepCopy.tap(_.*=(d))

    end *

    inline def +(d: Float): Matrix[Float] =
      m.deepCopy.tap(_.+=(d))
    end +

  end extension

  extension (d: Float)
    inline def *(m: Matrix[Float])(using inline boundsCheck: BoundsCheck): Matrix[Float] = m * d

    inline def +(m: Matrix[Float])(using inline boundsCheck: BoundsCheck): Matrix[Float] = m + d

    inline def -(m: Matrix[Float])(using inline boundsCheck: BoundsCheck): Matrix[Float] = ???
    inline def /(m: Matrix[Float])(using inline boundsCheck: BoundsCheck): Matrix[Float] = ???

    inline def *=(m: Matrix[Float])(using inline boundsCheck: BoundsCheck): Unit = m *= d
    inline def +=(m: Matrix[Float])(using inline boundsCheck: BoundsCheck): Unit = ??? // m += d
    inline def -=(m: Matrix[Float])(using inline boundsCheck: BoundsCheck): Unit = ??? // m -= d
    inline def /=(m: Matrix[Float])(using inline boundsCheck: BoundsCheck): Unit = ???

  end extension

end JvmFloatMatrix

object NativeFloatMatrix:

end NativeFloatMatrix
