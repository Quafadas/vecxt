package vecxt

import vecxt.BoundsCheck.BoundsCheck
import vecxt.MatrixInstance.*
import vecxt.all.`matmulInPlace!`
import vecxt.dimensionExtender.DimensionExtender.*
import vecxt.doublearrays.*
import vecxt.matrix.*
import vecxt.matrixUtil.*

object DoubleMatrix:

  extension (d: Double)
    inline def *(m: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m * d
    inline def +(m: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m + d
    inline def -(m: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = ???
    inline def /(m: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = ???

    inline def *=(m: Matrix[Double])(using inline boundsCheck: BoundsCheck): Unit = m *= d
    inline def +=(m: Matrix[Double])(using inline boundsCheck: BoundsCheck): Unit = ??? // m += d
    inline def -=(m: Matrix[Double])(using inline boundsCheck: BoundsCheck): Unit = ??? // m -= d
    inline def /=(m: Matrix[Double])(using inline boundsCheck: BoundsCheck): Unit = ???

  end extension

  extension (m: Matrix[Double])

    inline def @@(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      m.matmul(b)

    inline def matmul(b: Matrix[Double], alpha: Double = 1.0, beta: Double = 0.0)(using
        inline boundsCheck: BoundsCheck
    ): Matrix[Double] =
      dimMatCheck(m, b)
      val newArr: Array[Double] = Array.ofDim[Double](m.rows * b.cols)
      val newmat = Matrix[Double](newArr, m.rows, b.cols)
      m.`matmulInPlace!`(b, newmat, alpha, beta)(using boundsCheck)
      newmat
    end matmul

    inline def *=(d: Double): Unit =
      if m.hasSimpleContiguousMemoryLayout then m.raw.multInPlace(d)
      else
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val idx = m.offset + i * m.rowStride + j * m.colStride
            m.raw(idx) = m.raw(idx) * d
            j += 1
          end while
          i += 1
        end while

    inline def *(n: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.doublearrays.*(m.raw)(n), m.rows, m.cols, m.rowStride, m.colStride, m.offset)(using
          BoundsCheck.DoBoundsCheck.no
        )
      else ???
    end *

    inline def /(n: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.doublearrays./(m.raw)(n), m.rows, m.cols, m.rowStride, m.colStride, m.offset)(using
          BoundsCheck.DoBoundsCheck.no
        )
      else
        val newArr = Array.ofDim[Double](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = m.raw(srcIdx) / n
            j += 1
          end while
          i += 1
        end while
        Matrix[Double](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)
    end /

    inline def +(n: Double): Matrix[Double] =
      import vecxt.BoundsCheck.DoBoundsCheck.no
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.doublearrays.+(m.raw)(n), m.rows, m.cols, m.rowStride, m.colStride, m.offset)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.raw.copyToArray(newArr)
        val newMat = Matrix[Double](newArr, m.rows, m.cols, m.rowStride, m.colStride, m.offset)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            newMat(i, j) = m(i, j) + n
            j += 1
          end while
          i += 1
        end while
        newMat
      end if

    end +

    inline def maximum(other: Matrix[Double])(using inline boundsCheck: BoundsCheck) =
      sameDimMatCheck(m, other)
      val newArr = Array.ofDim[Double](m.numel)

      // TODO: SIMD optimization
      if sameDenseElementWiseMemoryLayoutCheck(m, other) then
        var i = 0
        while i < m.numel do
          newArr(i) = math.max(m.raw(i), other.raw(i))
          i += 1
        end while
      else
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val idx = i * m.rowStride + j * m.colStride + m.offset
            val idxOther = i * other.rowStride + j * other.colStride + other.offset
            newArr(i * m.cols + j) = math.max(m.raw(idx), other.raw(idxOther))
            j += 1
          end while
          i += 1
        end while
      end if
      Matrix[Double](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)
    end maximum

    inline def -(n: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.doublearrays.-(m.raw)(n), m.rows, m.cols, m.rowStride, m.colStride, m.offset)(using
          BoundsCheck.DoBoundsCheck.no
        )
      else
        import vecxt.BoundsCheck.DoBoundsCheck.no
        val newArr = Array.ofDim[Double](m.numel)
        m.raw.copyToArray(newArr)
        val newMat = Matrix[Double](newArr, m.rows, m.cols, m.rowStride, m.colStride, m.offset)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            newMat(i, j) = m(i, j) - n
            j += 1
          end while
          i += 1
        end while
        newMat
    end -

    // TODO: +:+=
    // TODO: SIMD on JVM
    inline def +:+(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.doublearrays.+(m.raw)(m2.raw)
        Matrix[Double](newArr, m.rows, m.cols, m.rowStride, m.colStride, m.offset)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.raw.copyToArray(newArr)
        val newMat = Matrix[Double](newArr, m.rows, m.cols, m.rowStride, m.colStride, m.offset)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            newMat(i, j) = newMat(i, j) + m2(i, j)
            j += 1
          end while
          i += 1
        end while
        newMat
      end if
    end +:+

    inline def +(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m +:+ m2

    inline def *(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m.hadamard(m2)

    inline def kronecker(other: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = ???

    inline def hadamard(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, m2)

      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        // Fast path: use SIMD-optimized array multiplication
        val newArr = vecxt.doublearrays.*(m.raw)(m2.raw)
        Matrix[Double](newArr, m.rows, m.cols, m.rowStride, m.colStride, m.offset)(using BoundsCheck.DoBoundsCheck.no)
      else
        // Different memory layouts: materialize one matrix to match the other's layout
        if m.isDenseColMajor then
          // m is dense column-major, materialize m2 to column-major and multiply in-place
          val m2Dense = m2.deepCopy(asRowMajor = false)
          vecxt.doublearrays.*:*=(m2Dense.raw)(m.raw)
          m2Dense
        else if m.isDenseRowMajor then
          // m is dense row-major, materialize m2 to row-major and multiply in-place
          val m2Dense = m2.deepCopy(asRowMajor = true)
          vecxt.doublearrays.*=(m2Dense.raw)(m.raw)
          m2Dense
        else if m2.isDenseColMajor then
          // m2 is dense column-major, materialize m to column-major and multiply in-place
          val mDense = m.deepCopy(asRowMajor = false)
          vecxt.doublearrays.*=(mDense.raw)(m2.raw)
          mDense
        else if m2.isDenseRowMajor then
          // m2 is dense row-major, materialize m to row-major and multiply in-place
          val mDense = m.deepCopy(asRowMajor = true)
          vecxt.doublearrays.*=(mDense.raw)(m2.raw)
          mDense
        else
          // Neither is dense, materialize both to column-major and use SIMD multiplication
          val mDense = m.deepCopy(asRowMajor = false)
          val m2Dense = m2.deepCopy(asRowMajor = false)
          val newArr = vecxt.doublearrays.*(mDense.raw)(m2Dense.raw)
          Matrix[Double](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)
        end if
      end if
    end hadamard

    inline def /:/(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.doublearrays./(m.raw)(m2.raw)
        Matrix[Double](newArr, m.rows, m.cols, m.rowStride, m.colStride, m.offset)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Double](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val mIdx = m.offset + i * m.rowStride + j * m.colStride
            val m2Idx = m2.offset + i * m2.rowStride + j * m2.colStride
            newArr(i + j * m.rows) = m.raw(mIdx) / m2.raw(m2Idx)
            j += 1
          end while
          i += 1
        end while
        Matrix[Double](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)
      end if
    end /:/

    // TODO: -:-=
    // TODO: SIMD on JVM
    inline def -:-(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.doublearrays.-(m.raw)(m2.raw)
        Matrix[Double](newArr, m.rows, m.cols, m.rowStride, m.colStride, m.offset)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.raw.copyToArray(newArr)
        val newMat = Matrix[Double](newArr, m.rows, m.cols, m.rowStride, m.colStride, m.offset)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            newMat(i, j) = newMat(i, j) - m2(i, j)
            j += 1
          end while
          i += 1
        end while
        newMat
      end if
    end -:-
    inline def -(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m -:- m2

    inline def unary_- : Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.doublearrays.unary_-(m.raw), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    inline def `exp!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`exp!`(m.raw)
      else
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val idx = m.offset + i * m.rowStride + j * m.colStride
            m.raw(idx) = Math.exp(m.raw(idx))
            j += 1
          end while
          i += 1
        end while

    inline def `log!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`log!`(m.raw)
      else
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val idx = m.offset + i * m.rowStride + j * m.colStride
            m.raw(idx) = Math.log(m.raw(idx))
            j += 1
          end while
          i += 1
        end while

    inline def exp: Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.all.exp(m.raw), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Double](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = Math.exp(m.raw(srcIdx))
            j += 1
          end while
          i += 1
        end while
        Matrix[Double](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)

    inline def log: Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.all.log(m.raw), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else
        // allocate a fresh column-major matrix (rowStride=1, colStride=rows)
        val newArr = Array.ofDim[Double](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = Math.log(m.raw(srcIdx))
            j += 1
          end while
          i += 1
        end while
        Matrix[Double](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)

    inline def `sqrt!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`sqrt!`(m.raw)
      else
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val idx = m.offset + i * m.rowStride + j * m.colStride
            m.raw(idx) = Math.sqrt(m.raw(idx))
            j += 1
          end while
          i += 1
        end while

    inline def sqrt: Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.all.sqrt(m.raw), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Double](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = Math.sqrt(m.raw(srcIdx))
            j += 1
          end while
          i += 1
        end while
        Matrix[Double](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)

    inline def sin =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.all.sin(m.raw), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Double](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = Math.sin(m.raw(srcIdx))
            j += 1
          end while
          i += 1
        end while
        Matrix[Double](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)

    inline def `sin!` =
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`sin!`(m.raw)
      else
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val idx = m.offset + i * m.rowStride + j * m.colStride
            m.raw(idx) = Math.sin(m.raw(idx))
            j += 1
          end while
          i += 1
        end while

    inline def cos =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.all.cos(m.raw), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Double](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = Math.cos(m.raw(srcIdx))
            j += 1
          end while
          i += 1
        end while
        Matrix[Double](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)

    inline def `cos!` = vecxt.doublearrays.`cos!`(m.raw)

    inline def tan =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.all.tan(m.raw), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    inline def `tan!` =
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`tan!`(m.raw)
      else ???

    inline def mean: Double =
      if m.hasSimpleContiguousMemoryLayout then m.sumSIMD / (m.rows * m.cols)
      else ???

    inline def **(power: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.all.**(m.raw)(power), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    private inline def reduceAlongDimension(
        dim: DimensionExtender,
        inline op: (Double, Double) => Double,
        inline initial: Double
    ): Matrix[Double] =
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

      Matrix[Double](newArr, newShape)(using BoundsCheck.DoBoundsCheck.no)
    end reduceAlongDimension

    inline def max(dim: DimensionExtender): Matrix[Double] =
      reduceAlongDimension(dim, math.max, Double.MinValue)
    end max

    inline def min(dim: DimensionExtender): Matrix[Double] =
      reduceAlongDimension(dim, math.min, Double.MaxValue)
    end min

    inline def sum(dim: DimensionExtender): Matrix[Double] =
      reduceAlongDimension(dim, _ + _, 0.0)
    end sum

    inline def product(dim: DimensionExtender): Matrix[Double] =
      reduceAlongDimension(dim, _ * _, 1.0)
    end product

    // inline def - : Matrix[Double] =
    //   Matrix(vecxt.doublearrays.*(m.raw)(-1), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def trace =
      if m.shape(0) != m.shape(1) then throw new IllegalArgumentException("Matrix must be square")
      end if
      m.diag.sum
    end trace

    inline def sum: Double = sumSIMD

    inline def sumSIMD: Double =
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.sum(m.raw)
      else ???

    inline def norm: Double =
      if m.hasSimpleContiguousMemoryLayout then vecxt.all.norm(m.raw)
      else ???

    // Note: det method is provided by platform-specific implementations
    // See: vecxt.JvmDeterminant (JVM with SIMD) and vecxt.JsNativeDeterminant (JS/Native)

    // inline def >=(d: Double): Matrix[Boolean] =

    // inline def >=(d: Double): Matrix[Boolean] =
    //   Matrix[Boolean](m.raw >= d, m.shape)(using BoundsCheck.DoBoundsCheck.no)

    // inline def >(d: Double): Matrix[Boolean] =
    //   Matrix(m.raw.gt(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    // inline def <=(d: Double): Matrix[Boolean] =
    //   Matrix(m.raw.lte(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    // inline def <(d: Double): Matrix[Boolean] =
    //   Matrix(m.raw.lt(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

  end extension
end DoubleMatrix
