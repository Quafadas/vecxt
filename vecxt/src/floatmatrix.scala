package vecxt

import vecxt.MatrixInstance.*
import vecxt.matrix.*
import vecxt.matrixUtil.*

import scala.annotation.targetName

/** Cross-platform, non-SIMD `Matrix[Float]` operations, the `Float` counterpart of `DoubleMatrix`. Platform-specific
  * SIMD/BLAS-backed operations (e.g. `matmul`, scalar `*`/`+`, comparisons) live in `JvmFloatMatrix` /
  * `JsFloatMatrix` / `NativeFloatMatrix` instead - this file only adds what none of those three currently define, to
  * avoid ambiguous extension method clashes on the JVM target.
  */
object FloatMatrix:

  extension (m: Matrix[Float])

    @targetName("floatMatrixMaximum")
    def maximum(other: Matrix[Float]): Matrix[Float] =
      sameDimMatCheck(m, other)

      // TODO: SIMD optimization
      if sameDenseElementWiseMemoryLayoutCheck(m, other) then
        val newArr = Array.ofDim[Float](m.numel)
        var i = 0
        val bound = m.numel
        while i < bound do
          newArr(i) = math.max(m.raw(i), other.raw(i))
          i += 1
        end while
        Matrix(newArr, m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          val idxOther = other.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = math.max(m.raw(idx), other.raw(idxOther))
        }
        Matrix[Float](newArr, m.rows, m.cols)
      end if
    end maximum

    def /=(n: Float): Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.floatarrays./=(m.raw)(n)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = m.raw(idx) / n
        }
      end if
    end /=

    /** Elementwise scalar divide. Layout policy: see `Matrix[Double]#*(n: Double)`. */
    def /(n: Float): Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.floatarrays./(m.raw)(n), m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        val asRowMajor = m.layout.unitStrideAxis == 1
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(if asRowMajor then i * m.cols + j else i + j * m.rows) = m.raw(srcIdx) / n
        }
        if asRowMajor then Matrix[Float](newArr, m.rows, m.cols, m.cols, 1, 0)
        else Matrix[Float](newArr, m.rows, m.cols, 1, m.rows, 0)
        end if
      end if
    end /

    /** Elementwise scalar subtract. Layout policy: see `Matrix[Double]#*(n: Double)`. */
    def -(n: Float): Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.floatarrays.-(m.raw)(n), m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        val asRowMajor = m.layout.unitStrideAxis == 1
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(if asRowMajor then i * m.cols + j else i + j * m.rows) = m.raw(srcIdx) - n
        }
        if asRowMajor then Matrix[Float](newArr, m.rows, m.cols, m.cols, 1, 0)
        else Matrix[Float](newArr, m.rows, m.cols, 1, m.rows, 0)
        end if
    end -

    // TODO: +:+=
    // TODO: SIMD on JVM
    @targetName("floatMatrixPlusPlus")
    def +:+(m2: Matrix[Float]): Matrix[Float] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.floatarrays.+(m.raw)(m2.raw)
        Matrix(newArr, m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val m2Idx = m2.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(mIdx) + m2.raw(m2Idx)
        }
        Matrix[Float](newArr, m.rows, m.cols)
      end if
    end +:+

    @targetName("floatMatrixPlusMatrix")
    def +(m2: Matrix[Float]): Matrix[Float] = m +:+ m2

    @targetName("floatMatrixHadamardOperator")
    def *(m2: Matrix[Float]): Matrix[Float] = m.hadamard(m2)

    @targetName("floatMatrixKronecker")
    def kronecker(other: Matrix[Float]): Matrix[Float] = ???

    /** Elementwise multiply. `-:-`'s slower cousin, a full elementwise op rather than a delegate, because unlike
      * `-`/`-:-` there is no existing platform-specific `Matrix[Float] * Matrix[Float]` to reuse - see the module
      * doc comment above.
      */
    @targetName("floatMatrixHadamard")
    def hadamard(m2: Matrix[Float]): Matrix[Float] =
      sameDimMatCheck(m, m2)

      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        // Fast path: use SIMD-optimized array multiplication
        val newArr = vecxt.floatarrays.*(m.raw)(m2.raw)
        Matrix(newArr, m.layout)
      else
        // Different memory layouts: materialize one matrix to match the other's layout - see
        // `DoubleMatrix#hadamard` for the reasoning behind each branch below.
        if m.hasSimpleContiguousMemoryLayout && m.isDenseColMajor then
          val m2Dense = m2.deepCopy(asRowMajor = false)
          vecxt.floatarrays.*=(m2Dense.raw)(m.raw)
          m2Dense
        else if m.hasSimpleContiguousMemoryLayout && m.isDenseRowMajor then
          val m2Dense = m2.deepCopy(asRowMajor = true)
          vecxt.floatarrays.*=(m2Dense.raw)(m.raw)
          m2Dense
        else if m2.hasSimpleContiguousMemoryLayout && m2.isDenseColMajor then
          val mDense = m.deepCopy(asRowMajor = false)
          vecxt.floatarrays.*=(mDense.raw)(m2.raw)
          mDense
        else if m2.hasSimpleContiguousMemoryLayout && m2.isDenseRowMajor then
          val mDense = m.deepCopy(asRowMajor = true)
          vecxt.floatarrays.*=(mDense.raw)(m2.raw)
          mDense
        else
          val mDense = m.deepCopy(asRowMajor = false)
          val m2Dense = m2.deepCopy(asRowMajor = false)
          val newArr = vecxt.floatarrays.*(mDense.raw)(m2Dense.raw)
          Matrix[Float](newArr, m.rows, m.cols)
        end if
      end if
    end hadamard

    @targetName("floatMatrixDivDiv")
    def /:/(m2: Matrix[Float]): Matrix[Float] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.floatarrays./(m.raw)(m2.raw)
        Matrix(newArr, m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val m2Idx = m2.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(mIdx) / m2.raw(m2Idx)
        }
        Matrix[Float](newArr, m.rows, m.cols)
      end if
    end /:/

    /** Elementwise matrix subtract, the `Float` counterpart of `DoubleMatrix#-:-`. (Not a delegate to
      * `Matrix[Float]#-`, which is instead implemented directly in `JvmFloatMatrix`/`JsFloatMatrix`/
      * `NativeFloatMatrix` and not visible from this cross-platform file.)
      */
    @targetName("floatMatrixMinusMinus")
    def -:-(m2: Matrix[Float]): Matrix[Float] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.floatarrays.-(m.raw)(m2.raw)
        Matrix(newArr, m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val m2Idx = m2.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(mIdx) - m2.raw(m2Idx)
        }
        Matrix[Float](newArr, m.rows, m.cols)
      end if
    end -:-

    @targetName("floatMatrixUnaryMinus")
    def unary_- : Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Float](vecxt.floatarrays.unary_-(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = -m.raw(srcIdx)
        }
        Matrix[Float](newArr, m.rows, m.cols)

    @targetName("floatMatrixExpInPlace")
    def `exp!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.floatarrays.`exp!`(m.raw)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = Math.exp(m.raw(idx)).toFloat
        }

    @targetName("floatMatrixLogInPlace")
    def `log!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.floatarrays.`log!`(m.raw)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = Math.log(m.raw(idx)).toFloat
        }

    @targetName("floatMatrixExp")
    def exp: Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Float](vecxt.all.exp(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = Math.exp(m.raw(srcIdx)).toFloat
        }
        Matrix[Float](newArr, m.rows, m.cols)

    @targetName("floatMatrixLog")
    def log: Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Float](vecxt.all.log(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = Math.log(m.raw(srcIdx)).toFloat
        }
        Matrix[Float](newArr, m.rows, m.cols)

    @targetName("floatMatrixSqrtInPlace")
    def `sqrt!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.floatarrays.`sqrt!`(m.raw)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = Math.sqrt(m.raw(idx).toDouble).toFloat
        }
      end if
    end `sqrt!`

    @targetName("floatMatrixSqrt")
    def sqrt: Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Float](vecxt.all.sqrt(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = Math.sqrt(m.raw(srcIdx).toDouble).toFloat
        }
        Matrix[Float](newArr, m.rows, m.cols)

    @targetName("floatMatrixSin")
    def sin: Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Float](vecxt.all.sin(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = Math.sin(m.raw(srcIdx).toDouble).toFloat
        }
        Matrix[Float](newArr, m.rows, m.cols)

    @targetName("floatMatrixSinInPlace")
    def `sin!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.floatarrays.`sin!`(m.raw)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = Math.sin(m.raw(idx).toDouble).toFloat
        }
      end if
    end `sin!`

    @targetName("floatMatrixCos")
    def cos: Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Float](vecxt.all.cos(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = Math.cos(m.raw(srcIdx).toDouble).toFloat
        }
        Matrix[Float](newArr, m.rows, m.cols)

    @targetName("floatMatrixCosInPlace")
    def `cos!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.floatarrays.`cos!`(m.raw)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = Math.cos(m.raw(idx).toDouble).toFloat
        }
      end if
    end `cos!`

    @targetName("floatMatrixTan")
    def tan: Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Float](vecxt.all.tan(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = Math.tan(m.raw(srcIdx).toDouble).toFloat
        }
        Matrix[Float](newArr, m.rows, m.cols)

    @targetName("floatMatrixTanInPlace")
    def `tan!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.floatarrays.`tan!`(m.raw)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = Math.tan(m.raw(idx).toDouble).toFloat
        }
      end if
    end `tan!`

    def mean: Float =
      if m.hasSimpleContiguousMemoryLayout then m.sumSIMD / (m.rows * m.cols)
      else
        var acc = 0.0f
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          acc += m.raw(idx)
        }
        acc / (m.rows * m.cols)
      end if
    end mean

    def **(power: Float): Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Float](vecxt.all.**(m.raw)(power), m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = Math.pow(m.raw(srcIdx).toDouble, power.toDouble).toFloat
        }
        Matrix[Float](newArr, m.rows, m.cols)
      end if
    end **

    def trace: Float =
      if m.shape(0) != m.shape(1) then throw new IllegalArgumentException("Matrix must be square")
      end if
      m.diag.sum
    end trace

    inline def sum: Float = sumSIMD

    def sumSIMD: Float =
      if m.hasSimpleContiguousMemoryLayout then vecxt.floatarrays.sum(m.raw)
      else
        var acc = 0.0f
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          acc += m.raw(idx)
        }
        acc
      end if
    end sumSIMD

    def norm: Float =
      if m.hasSimpleContiguousMemoryLayout then vecxt.all.norm(m.raw)
      else
        var acc = 0.0f
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          acc += m.raw(idx) * m.raw(idx)
        }
        Math.sqrt(acc.toDouble).toFloat
      end if
    end norm

    // Note: det method is provided by platform-specific implementations
    // See: vecxt.JvmDeterminant (JVM with SIMD) and vecxt.JsNativeDeterminant (JS/Native)

  end extension
end FloatMatrix
