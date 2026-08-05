package vecxt

import vecxt.MatrixInstance.*
import vecxt.all.`matmulInPlace!`
import vecxt.dimensionExtender.DimensionExtender.*
import vecxt.doublearrays.*
import vecxt.matrix.*
import vecxt.matrixUtil.*

object DoubleMatrix:

  extension (d: Double)
    def *(m: Matrix[Double]): Matrix[Double] = m * d
    def +(m: Matrix[Double]): Matrix[Double] = m + d
    def -(m: Matrix[Double]): Matrix[Double] = ???
    def /(m: Matrix[Double]): Matrix[Double] = ???

    def *=(m: Matrix[Double]): Unit = m *= d
    def +=(m: Matrix[Double]): Unit = ??? // m += d
    def -=(m: Matrix[Double]): Unit = ??? // m -= d
    def /=(m: Matrix[Double]): Unit = ???

  end extension

  extension (m: Matrix[Double])

    def @@(b: Matrix[Double]): Matrix[Double] =
      m.matmul(b)

    def matmul(b: Matrix[Double], alpha: Double = 1.0, beta: Double = 0.0): Matrix[Double] =
      dimMatCheck(m, b)
      val newArr: Array[Double] = Array.ofDim[Double](m.rows * b.cols)
      val newmat = Matrix[Double](newArr, m.rows, b.cols)
      m.`matmulInPlace!`(b, newmat, alpha, beta)
      newmat
    end matmul

    def *=(d: Double): Unit =
      if m.hasSimpleContiguousMemoryLayout then m.raw.multInPlace(d)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = m.raw(idx) * d
        }

    def *(n: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.doublearrays.*(m.raw)(n), m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) * n
        }
        Matrix[Double](newArr, m.rows, m.cols)
    end *

    def /(n: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.doublearrays./(m.raw)(n), m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) / n
        }
        Matrix[Double](newArr, m.rows, m.cols)
    end /

    def +(n: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.doublearrays.+(m.raw)(n), m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) + n
        }
        Matrix[Double](newArr, m.rows, m.cols)
      end if

    end +

    def maximum(other: Matrix[Double]) =
      sameDimMatCheck(m, other)

      // TODO: SIMD optimization
      if sameDenseElementWiseMemoryLayoutCheck(m, other) then
        val newArr = Array.ofDim[Double](m.numel)
        var i = 0
        val bound = m.numel
        while i < bound do
          newArr(i) = math.max(m.raw(i), other.raw(i))
          i += 1
        end while
        // newArr is filled in m's own element order (row- or col-major), so it must be wrapped with m's
        // layout, not always assumed column-major — see `+:+` for the same pattern.
        Matrix(newArr, m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          val idxOther = other.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = math.max(m.raw(idx), other.raw(idxOther))
        }
        Matrix[Double](newArr, m.rows, m.cols)
      end if
    end maximum

    def -(n: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.doublearrays.-(m.raw)(n), m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) - n
        }
        Matrix[Double](newArr, m.rows, m.cols)
    end -

    // TODO: +:+=
    // TODO: SIMD on JVM
    def +:+(m2: Matrix[Double]): Matrix[Double] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.doublearrays.+(m.raw)(m2.raw)
        Matrix(newArr, m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val m2Idx = m2.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(mIdx) + m2.raw(m2Idx)
        }
        Matrix[Double](newArr, m.rows, m.cols)
      end if
    end +:+

    def +(m2: Matrix[Double]): Matrix[Double] = m +:+ m2

    def *(m2: Matrix[Double]): Matrix[Double] = m.hadamard(m2)

    def kronecker(other: Matrix[Double]): Matrix[Double] = ???

    def hadamard(m2: Matrix[Double]): Matrix[Double] =
      sameDimMatCheck(m, m2)

      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        // Fast path: use SIMD-optimized array multiplication
        val newArr = vecxt.doublearrays.*(m.raw)(m2.raw)
        Matrix(newArr, m.layout)
      else
        // Different memory layouts: materialize one matrix to match the other's layout
        if m.isDenseColMajor then
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
          Matrix[Double](newArr, m.rows, m.cols)
        end if
      end if
    end hadamard

    def /:/(m2: Matrix[Double]): Matrix[Double] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.doublearrays./(m.raw)(m2.raw)
        Matrix(newArr, m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val m2Idx = m2.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(mIdx) / m2.raw(m2Idx)
        }
        Matrix[Double](newArr, m.rows, m.cols)
      end if
    end /:/

    // TODO: SIMD on JVM
    def -:-(m2: Matrix[Double]): Matrix[Double] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.doublearrays.-(m.raw)(m2.raw)
        Matrix(newArr, m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val m2Idx = m2.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(mIdx) - m2.raw(m2Idx)
        }
        Matrix[Double](newArr, m.rows, m.cols)
      end if
    end -:-
    def -(m2: Matrix[Double]): Matrix[Double] = m -:- m2

    def unary_- : Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.doublearrays.unary_-(m.raw), m.layout)
      else ???

    def `exp!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`exp!`(m.raw)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = Math.exp(m.raw(idx))
        }

    def `log!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`log!`(m.raw)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = Math.log(m.raw(idx))
        }

    def exp: Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.exp(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = Math.exp(m.raw(srcIdx))
        }
        Matrix[Double](newArr, m.rows, m.cols)

    def log: Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.log(m.raw), m.layout)
      else
        // allocate a fresh column-major matrix (rowStride=1, colStride=rows)
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = Math.log(m.raw(srcIdx))
        }
        Matrix[Double](newArr, m.rows, m.cols)

    def `sqrt!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`sqrt!`(m.raw)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = Math.sqrt(m.raw(idx))
        }
      end if
    end `sqrt!`

    def sqrt: Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.sqrt(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = Math.sqrt(m.raw(srcIdx))
        }
        Matrix[Double](newArr, m.rows, m.cols)

    def sin =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.sin(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = Math.sin(m.raw(srcIdx))
        }
        Matrix[Double](newArr, m.rows, m.cols)

    def `sin!` =
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`sin!`(m.raw)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = Math.sin(m.raw(idx))
        }
      end if
    end `sin!`

    def cos =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.cos(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = Math.cos(m.raw(srcIdx))
        }
        Matrix[Double](newArr, m.rows, m.cols)

    def `cos!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`cos!`(m.raw)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = Math.cos(m.raw(idx))
        }
      end if
    end `cos!`

    def tan =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.tan(m.raw), m.layout)
      else ???

    def `tan!` =
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`tan!`(m.raw)
      else ???

    def mean: Double =
      if m.hasSimpleContiguousMemoryLayout then m.sumSIMD / (m.rows * m.cols)
      else ???

    def **(power: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.**(m.raw)(power), m.layout)
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

      Matrix[Double](newArr, newShape)
    end reduceAlongDimension

    def max(dim: DimensionExtender): Matrix[Double] =
      reduceAlongDimension(dim, math.max, Double.MinValue)
    end max

    def min(dim: DimensionExtender): Matrix[Double] =
      reduceAlongDimension(dim, math.min, Double.MaxValue)
    end min

    def sum(dim: DimensionExtender): Matrix[Double] =
      reduceAlongDimension(dim, _ + _, 0.0)
    end sum

    def product(dim: DimensionExtender): Matrix[Double] =
      reduceAlongDimension(dim, _ * _, 1.0)
    end product

    // inline def - : Matrix[Double] =
    //   Matrix(vecxt.doublearrays.*(m.raw)(-1), m.shape)

    def trace =
      if m.shape(0) != m.shape(1) then throw new IllegalArgumentException("Matrix must be square")
      end if
      m.diag.sum
    end trace

    def sum: Double = sumSIMD

    def sumSIMD: Double =
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.sum(m.raw)
      else ???

    def norm: Double =
      if m.hasSimpleContiguousMemoryLayout then vecxt.all.norm(m.raw)
      else ???

    // Note: det method is provided by platform-specific implementations
    // See: vecxt.JvmDeterminant (JVM with SIMD) and vecxt.JsNativeDeterminant (JS/Native)

    // inline def >=(d: Double): Matrix[Boolean] =

    // inline def >=(d: Double): Matrix[Boolean] =
    //   Matrix[Boolean](m.raw >= d, m.shape)

    // inline def >(d: Double): Matrix[Boolean] =
    //   Matrix(m.raw.gt(d), m.shape)
  // inline def <=(d: Double): Matrix[Boolean] =
  //   Matrix(m.raw.lte(d), m.shape)
  // inline def <(d: Double): Matrix[Boolean] =
  //   Matrix(m.raw.lt(d), m.shape)
  end extension
end DoubleMatrix
