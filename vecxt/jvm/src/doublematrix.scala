package vecxt

import scala.reflect.ClassTag

import vecxt.BoundsCheck.BoundsCheck
import all.*

import dev.ludovic.netlib.blas.JavaBLAS.getInstance as blas

object JvmDoubleMatrix:
  extension (m: Matrix[Double])

    // inline def /(n: Double): Matrix[Double] =
    //   Matrix(vecxt.arrays./(m.raw)(n), m.shape)(using BoundsCheck.DoBoundsCheck.no)

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

    // TODO: SIMD
    inline def *:*(bmat: Matrix[Boolean])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, bmat)
      val newArr = Array.ofDim[Double](m.rows * m.cols)
      var i = 0
      while i < newArr.length do
        newArr(i) = if bmat.raw(i) then m.raw(i) else 0.0
        i += 1
      end while
      Matrix(newArr, (m.rows, m.cols))
    end *:*

    // inline def @@(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m.matmul(b)

    // inline def *:*=(d: Double): Unit = m.raw.multInPlace(d)

    inline def *(vec: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] =
      val newArr = Array.ofDim[Double](m.rows)
      blas.dgemv(
        "N",
        m.rows,
        m.cols,
        1.0,
        m.raw,
        m.rows,
        vec,
        1,
        0.0,
        newArr,
        1
      )
      newArr
    end *

    inline def >=(d: Double): Matrix[Boolean] =
      Matrix[Boolean](vecxt.arrays.>=(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def >(d: Double): Matrix[Boolean] =
      Matrix[Boolean](vecxt.arrays.>(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def <=(d: Double): Matrix[Boolean] =
      Matrix[Boolean](vecxt.arrays.<=(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def <(d: Double): Matrix[Boolean] =
      Matrix[Boolean](vecxt.arrays.<(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

  end extension

end JvmDoubleMatrix

object NativeDoubleMatrix:

end NativeDoubleMatrix
