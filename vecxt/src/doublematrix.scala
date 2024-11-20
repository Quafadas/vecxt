package vecxt

import narr.*
import matrix.*
import vecxt.MatrixHelper.*
import vecxt.MatrixInstance.*
import vecxt.BoundsCheck.BoundsCheck
import vecxt.arrays.*
import vecxt.rangeExtender.MatrixRange.range
import vecxt.rangeExtender.MatrixRange.RangeExtender
import vecxt.JvmDoubleMatrix.*
import vecxt.JsDoubleMatrix.*
import vecxt.NativeDoubleMatrix.*

object DoubleMatrix:

  extension (m: Matrix[Double])

    inline def @@(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m.matmul(b)

    inline def *:*=(d: Double): Unit = m.raw.multInPlace(d)

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
