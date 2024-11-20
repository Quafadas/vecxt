package vecxt

import vecxt.BoundsCheck.BoundsCheck
import vecxt.JsDoubleMatrix.*
import vecxt.JvmDoubleMatrix.*
import vecxt.MatrixHelper.*
import vecxt.MatrixInstance.*
import vecxt.NativeDoubleMatrix.*
import vecxt.arrays.*
import vecxt.matrix.*
import vecxt.rangeExtender.MatrixRange.RangeExtender
import vecxt.rangeExtender.MatrixRange.range

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
