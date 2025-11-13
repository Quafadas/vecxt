package vecxt

import scala.reflect.ClassTag

import vecxt.BoundsCheck.BoundsCheck
import vecxt.matrix.*

import narr.*

object MatrixHelper:
  extension (m: Matrix.type)

    inline def fromRowsArray[@specialized(Double, Boolean, Int) A](
        a: NArray[NArray[A]]
    )(using inline boundsCheck: BoundsCheck, classTag: ClassTag[A]): Matrix[A] =
      Matrix.fromRows(a.toSeq*)
    end fromRowsArray

    inline def fromRows[@specialized(Double, Boolean, Int) A](
        a: NArray[A]*
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
      Matrix(newArr, rows, cols)
    end fromRows

    inline def fromColumns[@specialized(Double, Boolean, Int) A](
        a: NArray[A]*
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

    inline def fromColumnsArray[@specialized(Double, Boolean, Int) A](
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
    end fromColumnsArray

    transparent inline def eye[A: ClassTag](dim: Int)(using inline onz: OneAndZero[A]): Matrix[A] =
      eyeOf(onz.one, dim)(onz.zero)

    transparent inline def eye[A: ClassTag](dim: RowCol)(using inline onz: OneAndZero[A]): Matrix[A] =
      eyeOf(onz.one, dim)(onz.zero)

    transparent inline def ones[A: ClassTag](dim: RowCol)(using inline onz: OneAndZero[A]): Matrix[A] =
      fill(onz.one, dim)

    transparent inline def fill[A](singleton: A, dim: RowCol)(using ClassTag[A]): Matrix[A] =
      val (rows, cols) = dim
      val newArr = NArray.fill[A](rows * cols)(singleton)
      Matrix(newArr, dim)(using BoundsCheck.DoBoundsCheck.no)
    end fill

    transparent inline def eyeOf[A: ClassTag](singleton: A, dim: Int)(zero: A): Matrix[A] =
      val size = dim * dim
      val newArr: NArray[A] = NArray.ofSize[A](size)
      var j = 0
      while j < size do
        newArr(j) = zero
        j += 1
      end while

      var i = 0
      while i < dim do
        newArr(i * dim + i) = singleton
        i += 1
      end while
      Matrix[A](newArr, (dim, dim))(using BoundsCheck.DoBoundsCheck.no)
    end eyeOf

    transparent inline def eyeOf[A: ClassTag: OneAndZero](singleton: A, row_col: RowCol)(zero: A): Matrix[A] =
      val size = row_col._1 * row_col._2
      val newArr: NArray[A] = NArray.ofSize[A](size)
      var j = 0
      while j < size do
        newArr(j) = zero
        j += 1
      end while

      var i = 0
      while i < row_col._1 do
        newArr(i * row_col._1 + i) = singleton
        i += 1
      end while
      Matrix[A](newArr, row_col)(using BoundsCheck.DoBoundsCheck.no)
    end eyeOf

    transparent inline def zeros[A: ClassTag](dim: RowCol)(using onz: OneAndZero[A]): Matrix[A] = fill(onz.zero, dim)

    transparent inline def zerosOf[A: ClassTag: OneAndZero](zero: A, dim: RowCol): Matrix[A] =
      val (rows, cols) = dim
      val size = rows * cols
      val newArr = NArray.ofSize[A](size)
      var j = 0
      while j < size do
        newArr(j) = zero
        j += 1
      end while
      Matrix(newArr, dim)(using BoundsCheck.DoBoundsCheck.no)
    end zerosOf

    inline def rand(rows: Int, cols: Int)(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      val size = rows * cols
      val newArr = NArray.ofSize[Double](size)
      val rng = new scala.util.Random()
      var i = 0
      while i < size do
        newArr(i) = rng.nextDouble()
        i += 1
      end while
      Matrix[Double](newArr, (rows, cols))
    end rand

    inline def rand(dim: RowCol)(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      rand(dim._1, dim._2)
    end rand

    inline def randInt(rows: Int, cols: Int, minVal: Int, maxVal: Int)(using
        inline boundsCheck: BoundsCheck
    ): Matrix[Int] =
      val size = rows * cols
      val newArr = NArray.ofSize[Int](size)
      val rng = new scala.util.Random()
      val range = maxVal - minVal
      var i = 0
      while i < size do
        newArr(i) = minVal + rng.nextInt(range)
        i += 1
      end while
      Matrix[Int](newArr, (rows, cols))
    end randInt

    inline def randInt(rows: Int, cols: Int)(using inline boundsCheck: BoundsCheck): Matrix[Int] =
      randInt(rows, cols, 0, 100)
    end randInt

    inline def randInt(dim: RowCol, minVal: Int, maxVal: Int)(using
        inline boundsCheck: BoundsCheck
    ): Matrix[Int] =
      randInt(dim._1, dim._2, minVal, maxVal)
    end randInt

    inline def randInt(dim: RowCol)(using inline boundsCheck: BoundsCheck): Matrix[Int] =
      randInt(dim._1, dim._2, 0, 100)
    end randInt
  end extension

end MatrixHelper
