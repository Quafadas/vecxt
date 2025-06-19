package vecxt

import vecxt.BoundsCheck.BoundsCheck
import vecxt.JvmDoubleMatrix.*
import vecxt.JsNativeDoubleArrays.*
import vecxt.arrays.*
import vecxt.matrix.*

// These are used in cross compilation.
import vecxt.JsDoubleMatrix.*
import vecxt.NativeDoubleMatrix.*

import vecxt.matrixUtil.diag
import scala.annotation.targetName

import narr.*
import dimensionExtender.DimensionExtender.*
import arrayUtil.printArr

object DoubleMatrix:

  extension (d: Double)
    inline def *(m: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.arrays.*(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
  end extension

  extension (m: Matrix[Double])

    inline def @@(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      if sameDenseElementWiseMemoryLayoutCheck(m, b) then m.matmul(b)
      else ???

    inline def *=(d: Double): Unit =
      if m.hasSimpleContiguousMemoryLayout then m.raw.multInPlace(d)
      else ???

    inline def *(n: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.arrays.*(m.raw)(n), m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end *

    inline def /(n: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.arrays./(m.raw)(n), m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end /

    inline def +(n: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.arrays.+(m.raw)(n), m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end +

    inline def -(n: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.arrays.-(m.raw)(n), m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end -

    inline def +:+(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.arrays.+(m.raw)(m2.raw)
        Matrix[Double](newArr, m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
      end if
    end +:+

    inline def +(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m +:+ m2

    inline def hadamard(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, m2)

      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.arrays.*(m.raw)(m2.raw)
        Matrix[Double](newArr, m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
      end if
    end hadamard

    inline def /:/(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.arrays./(m.raw)(m2.raw)
        Matrix[Double](newArr, m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
      end if
    end /:/

    inline def -:-(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.arrays.-(m.raw)(m2.raw)
        Matrix[Double](newArr, m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
      end if
    end -:-
    inline def -:(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m -:- m2

    inline def unary_- : Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.arrays.unary_-(m.raw), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    inline def `exp!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.arrays.`exp!`(m.raw)
      else ???

    inline def `log!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.arrays.`log!`(m.raw)
      else ???

    inline def exp: Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.all.exp(m.raw), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    inline def log: Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.all.log(m.raw), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    inline def `sqrt!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.arrays.`sqrt!`(m.raw)
      else ???

    inline def sqrt: Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.all.sqrt(m.raw), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    inline def sin =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.all.sin(m.raw), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    inline def `sin!` =
      if m.hasSimpleContiguousMemoryLayout then vecxt.arrays.`sin!`(m.raw)
      else ???

    inline def cos =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Double](vecxt.all.cos(m.raw), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    inline def `cos!` = vecxt.arrays.`cos!`(m.raw)

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

      val newArr = NArray.fill(newShape._1 * newShape._2)(initial)
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
    //   Matrix(vecxt.arrays.*(m.raw)(-1), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def trace =
      if m.shape(0) != m.shape(1) then throw new IllegalArgumentException("Matrix must be square")
      end if
      m.diag.sum
    end trace

    inline def sum: Double = sumSIMD
    inline def sumSIMD: Double =
      if m.hasSimpleContiguousMemoryLayout then vecxt.arrays.sum(m.raw)
      else ???

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
