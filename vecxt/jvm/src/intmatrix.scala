package vecxt

import scala.reflect.ClassTag

import vecxt.BoundsCheck.BoundsCheck
import vecxt.arrays.*
import vecxt.MatrixInstance.*
import vecxt.matrix.*

import dev.ludovic.netlib.blas.JavaBLAS.getInstance as blas

object JvmIntMatrix:
  extension (m: Matrix[Int])

    inline def matmul(b: Matrix[Int])(using inline boundsCheck: BoundsCheck): Matrix[Int] =
      dimMatCheck(m, b)
      ???

    end matmul

    inline def >=(d: Int): Matrix[Boolean] =
      val i: Array[Int] = m.raw
      Matrix[Boolean](m.raw.gte(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
    end >=

    inline def >(d: Int): Matrix[Boolean] =
      Matrix[Boolean](m.raw.gt(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def <=(d: Int): Matrix[Boolean] =
      Matrix[Boolean](m.raw.lte(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def <(d: Int): Matrix[Boolean] =
      Matrix[Boolean](m.raw.lt(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)

  end extension
end JvmIntMatrix

object NativeIntMatrix:

end NativeIntMatrix
