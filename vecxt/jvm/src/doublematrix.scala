package vecxt

import matrix.*
import narr.*
import vecxt.BoundsCheck.BoundsCheck
import vecxt.arrays.*
import dev.ludovic.netlib.blas.JavaBLAS.getInstance as blas
import scala.reflect.ClassTag
import vecxt.MatrixInstance.*

object DoubleMatrix:
  extension (m: Matrix[Double])

    inline def +(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, m2)
      val newArr = m.raw.add(m2.raw)
      Matrix(newArr, m.shape)(using BoundsCheck.DoBoundsCheck.no)
    end +

    inline def matmul(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      dimMatCheck(m, b)
      val newArr = Array.ofDim[Double](m.rows * b.cols)
      // Note, might need to deal with transpose later.
      blas.dgemm(
        "N",
        "N",
        m.rows,
        b.cols,
        m.cols,
        1.0,
        m.raw,
        m.rows,
        b.raw,
        b.rows,
        1.0,
        newArr,
        m.rows
      )
      Matrix(newArr, (m.rows, b.cols))
    end matmul

    // inline def @@(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m.matmul(b)

    // inline def *:*=(d: Double): Unit = m.raw.multInPlace(d)

    inline def >=(d: Double): Matrix[Boolean] =
      Matrix(m.raw.gte(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def >(d: Double): Matrix[Boolean] =
      Matrix(m.raw.gt(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def <=(d: Double): Matrix[Boolean] =
      Matrix(m.raw.lte(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def <(d: Double): Matrix[Boolean] =
      Matrix(m.raw.lt(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

  end extension

end DoubleMatrix
