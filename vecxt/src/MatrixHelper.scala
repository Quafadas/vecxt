package vecxt

import matrix.*
import narr.*
import vecxt.BoundsCheck.BoundsCheck
import vecxt.arrays.*
import scala.reflect.ClassTag
import scala.annotation.targetName
import scala.compiletime.*

object MatrixHelper:
  extension (m: Matrix.type)

    inline def zeros[@specialized(Double, Boolean, Int) A: ClassTag](dim: RowCol): Matrix[A] =
      val (rows, cols) = dim
      val newArr = NArray.ofSize[A](rows * cols)
      Matrix(newArr, dim)(using BoundsCheck.DoBoundsCheck.no)
    end zeros

    inline def fromRows[A](
        a: NArray[NArray[A]]
    )(using inline boundsCheck: BoundsCheck, classTag: ClassTag[A]): Matrix[A] =
      val rows = a.size
      val cols = a.head.size

      assert(a.forall(_.size == cols))

      val newArr = NArray.ofSize[A](rows * cols)
      var idx = 0
      var i = 0
      while i < cols do
        var j = 0
        while j < rows do
          newArr(idx) = a(j)(i)
          // println(s"row: $i || col: $j || ${a(j)(i)} entered at index ${idx}")
          j += 1
          idx += 1
        end while
        i += 1
      end while
      Matrix(newArr, (rows, cols))
    end fromRows

    inline def fromColumns[A](
        a: NArray[NArray[A]]
    )(using inline boundsCheck: BoundsCheck, classTag: ClassTag[A]): Matrix[A] =
      val cols = a.size
      val rows = a.head.size
      assert(a.forall(_.size == rows))
      val newArr = NArray.ofSize[A](rows * cols)
      var idx = 0
      var i = 0
      while i < cols do
        var j = 0
        while j < rows do
          newArr(idx) = a(i)(j)
          // println(s"i: $i || j: $j || ${a(i)(j)} entered at index ${idx}")
          j += 1
          idx += 1
        end while
        i += 1
      end while
      Matrix(newArr, (rows, cols))
    end fromColumns

    transparent inline def zero[A] =
      inline erasedValue[A] match
        case _: Double  => 0.0.asInstanceOf[A]
        case _: Boolean => false.asInstanceOf[A]
        case _: Int     => 0.asInstanceOf[A]
        case _: A       => error("Unsupported type")
      end match
    end zero

    transparent inline def one[A] =
      inline erasedValue[A] match
        case _: Double  => 1.0.asInstanceOf[A]
        case _: Boolean => true.asInstanceOf[A]
        case _: Int     => 1.asInstanceOf[A]
        case _: A       => error("Unsupported type")
      end match
    end one

    transparent inline def eye[A: ClassTag](dim: Int): Matrix[A] =
      inline erasedValue[A] match
        case _: Int     => eyeOf(one[A], dim)
        case _: Double  => eyeOf(one[A], dim)
        case _: Boolean => eyeOf(one[A], dim)
        case _          => error("Unsupported eye type")

    transparent inline def ones[A: ClassTag](dim: RowCol): Matrix[A] =
      inline erasedValue[A] match
        case _: Int     => fill(one[A], dim)
        case _: Double  => fill(one[A], dim)
        case _: Boolean => fill(one[A], dim)
        case _          => error("Unsupported type for ones")

    transparent inline def fill[A](singleton: A, dim: RowCol)(using ClassTag[A]): Matrix[A] =
      val (rows, cols) = dim
      val newArr = NArray.fill[A](rows * cols)(singleton)
      Matrix(newArr, dim)(using BoundsCheck.DoBoundsCheck.no)
    end fill

    transparent inline def eyeOf[A: ClassTag](singleton: A, dim: Int) =
      val newArr = NArray.ofSize[A](dim * dim)
      var i = 0
      while i < dim do
        newArr(i * dim + i) = singleton
        i += 1
      end while
      Matrix(newArr, (dim, dim))(using BoundsCheck.DoBoundsCheck.no)
    end eyeOf
  end extension

end MatrixHelper

//     inline def +(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
//       sameDimMatCheck(m, m2)
//       val newArr = m.raw.add(m2.raw)
//       Matrix(newArr, m.shape)(using BoundsCheck.DoBoundsCheck.no)
//     end +

//     inline def matmul(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
//       dimMatCheck(m, b)
//       val newArr = Array.ofDim[Double](m.rows * b.cols)
//       // Note, might need to deal with transpose later.
//       blas.dgemm(
//         "N",
//         "N",
//         m.rows,
//         b.cols,
//         m.cols,
//         1.0,
//         m.raw,
//         m.rows,
//         b.raw,
//         b.rows,
//         1.0,
//         newArr,
//         m.rows
//       )
//       Matrix(newArr, (m.rows, b.cols))
//     end matmul

//     inline def @@(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m.matmul(b)

//     inline def *:*=(d: Double): Unit = m.raw.multInPlace(d)

//   end extension

// end DoubleMatrix
