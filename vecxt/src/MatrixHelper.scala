package vecxt

import scala.reflect.ClassTag

import vecxt.BoundsCheck.BoundsCheck
import vecxt.MatrixInstance.apply
import vecxt.matrix.*

object MatrixHelper:
  extension (m: Matrix.type)

    inline def fromRowsArray[@specialized(Double, Boolean, Int) A](
        a: Array[Array[A]]
    )(using inline boundsCheck: BoundsCheck, classTag: ClassTag[A]): Matrix[A] =
      Matrix.fromRows(a.toSeq*)
    end fromRowsArray

    inline def fromRows[@specialized(Double, Boolean, Int) A](
        a: Array[A]*
    )(using inline boundsCheck: BoundsCheck, classTag: ClassTag[A]): Matrix[A] =
      val rows = a.size
      val cols = a.head.size

      assert(a.forall(_.size == cols))

      val newArr = Array.ofDim[A](rows * cols)
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
        a: Array[A]*
    )(using inline boundsCheck: BoundsCheck, classTag: ClassTag[A]): Matrix[A] =
      val cols = a.size
      val rows = a.head.size
      assert(a.forall(_.size == rows))
      val newArr = Array.ofDim[A](rows * cols)
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
        a: Array[Array[A]]
    )(using inline boundsCheck: BoundsCheck, classTag: ClassTag[A]): Matrix[A] =
      val cols = a.size
      val rows = a.head.size
      assert(a.forall(_.size == rows))
      val newArr = Array.ofDim[A](rows * cols)
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
      val newArr = Array.fill[A](rows * cols)(singleton)
      Matrix(newArr, dim)(using BoundsCheck.DoBoundsCheck.no)
    end fill

    transparent inline def eyeOf[A: ClassTag](singleton: A, dim: Int)(zero: A): Matrix[A] =
      val size = dim * dim
      val newArr: Array[A] = Array.ofDim[A](size)
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
      val newArr: Array[A] = Array.ofDim[A](size)
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
      val newArr = Array.ofDim[A](size)
      var j = 0
      while j < size do
        newArr(j) = zero
        j += 1
      end while
      Matrix(newArr, dim)(using BoundsCheck.DoBoundsCheck.no)
    end zerosOf

    inline def rand(rows: Int, cols: Int)(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      val size = rows * cols
      val newArr = Array.ofDim[Double](size)
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
      val newArr = Array.ofDim[Int](size)
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

    /** Tiles a matrix by repeating it in a grid pattern.
      *
      * Creates a new matrix by replicating the input matrix `rowsN` times vertically and `colsN` times horizontally.
      * The resulting matrix will have dimensions `(inM.rows * rowsN, inM.cols * colsN)`.
      *
      * @tparam A
      *   the type of elements in the matrix
      * @param inM
      *   the input matrix to be tiled
      * @param rowsN
      *   the number of times to repeat the matrix vertically
      * @param colsN
      *   the number of times to repeat the matrix horizontally
      * @return
      *   a new matrix containing the tiled pattern
      * @example
      *   {{{
      * val m = Matrix(Array(1, 2, 3, 4), 2, 2)  // [[1, 3], [2, 4]]
      * val tiled = tile(m, 2, 3)
      * // Results in a 4x6 matrix with m repeated 2 times vertically and 3 times horizontally
      *   }}}
      */
    inline def tile[A](inM: Matrix[A], rowsN: Int, colsN: Int)(using ClassTag[A]): Matrix[A] =
      import vecxt.BoundsCheck.DoBoundsCheck.no

      val newArr = Array.ofDim[A](inM.numel * rowsN * colsN)
      var r = 0
      while r < rowsN do
        var c = 0
        while c < colsN do
          var i = 0
          while i < inM.rows do
            var j = 0
            while j < inM.cols do
              val destRow = r * inM.rows + i
              val destCol = c * inM.cols + j
              newArr(destCol * (inM.rows * rowsN) + destRow) = inM(i, j)
              j += 1
            end while
            i += 1
          end while
          c += 1
        end while
        r += 1
      end while

      Matrix(newArr, inM.rows * rowsN, inM.cols * colsN)
    end tile

    inline def createDiagonal(v: Array[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      val size = v.length
      val newArr = Array.ofDim[Double](size * size)
      var j = 0
      while j < newArr.length do
        newArr(j) = 0.0
        j += 1
      end while

      var i = 0
      while i < size do
        newArr(i * size + i) = v(i)
        i += 1
      end while
      Matrix[Double](newArr, (size, size))
    end createDiagonal

  end extension

end MatrixHelper
