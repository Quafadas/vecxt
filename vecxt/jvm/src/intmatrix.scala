package vecxt

import scala.reflect.ClassTag

import vecxt.BoundsCheck.BoundsCheck
import vecxt.arrays.*
import vecxt.MatrixInstance.*
import vecxt.matrix.*

object JvmIntMatrix:
  extension (matA: Matrix[Int])

    inline def matmul(matB: Matrix[Int])(using inline boundsCheck: BoundsCheck): Matrix[Int] =
      dimMatCheck(matA, matB)
      val newArr2 = Array.fill[Int](matA.rows * matB.cols)(0)

      var i = 0
      while i < matA.rows do
        var j = 0
        while j < matB.cols do
          var sum = 0
          var k = 0
          while k < matA.cols do
            sum += matA.raw(i * matA.cols + k) * matB.raw(k * matB.cols + j)
            k += 1
          end while
          newArr2(i * matB.cols + j) = sum
          j += 1
        end while
        i += 1
      end while

      Matrix(newArr2, (matA.rows, matB.cols))

    end matmul

    inline def >=(d: Int): Matrix[Boolean] =
      val i: Array[Int] = matA.raw
      Matrix[Boolean](matA.raw.gte(d), matA.shape)(using BoundsCheck.DoBoundsCheck.no)
    end >=

    inline def >(d: Int): Matrix[Boolean] =
      Matrix[Boolean](matA.raw.gt(d), matA.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def <=(d: Int): Matrix[Boolean] =
      Matrix[Boolean](matA.raw.lte(d), matA.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def <(d: Int): Matrix[Boolean] =
      Matrix[Boolean](matA.raw.lt(d), matA.shape)(using BoundsCheck.DoBoundsCheck.no)

  end extension
end JvmIntMatrix

object NativeIntMatrix:

end NativeIntMatrix
