package vecxt

import vecxt.BoundsCheck.BoundsCheck
import vecxt.JvmDoubleMatrix.*
import vecxt.arrays.*
import vecxt.matrix.*

// These are used in cross compilation.
import vecxt.JsDoubleMatrix.*
import vecxt.NativeDoubleMatrix.*

import vecxt.matrixUtil.diag
import scala.annotation.targetName

import narr.*

object DoubleMatrix:

  extension (m: Matrix[Double])

    inline def @@(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m.matmul(b)

    inline def *:*=(d: Double): Unit = m.raw.multInPlace(d)

    inline def *(n: Double): Matrix[Double] =
      Matrix(vecxt.arrays.*(m.raw)(n), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def /(n: Double): Matrix[Double] =
      Matrix(vecxt.arrays./(m.raw)(n), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def +(n: Double): Matrix[Double] =
      Matrix(vecxt.arrays.+(m.raw)(n), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def +(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, m2)
      val newArr = m.raw.add(m2.raw)
      Matrix(newArr, m.shape)(using BoundsCheck.DoBoundsCheck.no)
    end +

    inline def -(n: Double): Matrix[Double] =
      Matrix(vecxt.arrays.-(m.raw)(n), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    // inline def - : Matrix[Double] =
    //   Matrix(vecxt.arrays.*(m.raw)(-1), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def trace =
      if m.shape(0) != m.shape(1) then throw new IllegalArgumentException("Matrix must be square")
      end if
      m.diag.sum
    end trace

    inline def sum = m.raw.sum

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
