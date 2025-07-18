package vecxt

import scala.reflect.ClassTag

import BoundsCheck.BoundsCheck
import matrix.*
import MatrixInstance.*

import narr.*

import scala.util.chaining.*
// import vecxt.arrayUtil.printArr
object matrixUtil:
  enum Vertical:
    case Top, Bottom
  end Vertical

  enum Horizontal:
    case Left, Right
  end Horizontal

  extension [@specialized(Double, Boolean, Int) A](m: Matrix[A])

    private inline def tupleFromIdx(b: Int)(using inline boundsCheck: BoundsCheck): RowCol =
      // dimCheckLen(m.raw, b)
      (b / m.rows, b % m.rows)
    end tupleFromIdx

    /** There should be
      *
      * @param f
      */
    inline def mapRowsInPlace(
        inline f: NArray[A] => NArray[A]
    )(using ClassTag[A]): Unit =
      import vecxt.BoundsCheck.DoBoundsCheck.no
      var idx = 0
      while idx < m.rows do
        m.updateInPlace(NArray[Int](idx), ::, f(m.row(idx)))
        idx += 1
      end while
    end mapRowsInPlace

    inline def mapRows[B](
        inline f: NArray[A] => NArray[B]
    )(using ClassTag[B], ClassTag[A]): Matrix[B] =
      import vecxt.BoundsCheck.DoBoundsCheck.no
      val newArr = NArray.ofSize[B](m.numel)
      val m2 = Matrix(newArr, m.rows, m.cols)
      var idx = 0
      while idx < m.rows do
        m2.updateInPlace(NArray[Int](idx), ::, f(m.row(idx)))
        idx += 1
      end while
      m2
    end mapRows

    inline def mapRowsToScalar[B](
        inline f: NArray[A] => B
    )(using ClassTag[B], ClassTag[A]): Matrix[B] =
      import vecxt.BoundsCheck.DoBoundsCheck.no
      val newArr = NArray.ofSize[B](m.rows)
      var i = 0
      while i < m.rows do
        newArr(i) = f(m.row(i))
        i += 1
      end while
      Matrix(newArr, (m.rows, 1))
    end mapRowsToScalar

    inline def mapColsInPlace(
        inline f: NArray[A] => NArray[A]
    )(using ClassTag[A]): Unit =
      import vecxt.BoundsCheck.DoBoundsCheck.no

      var idx = 0
      while idx < m.cols do
        m.updateInPlace(::, NArray[Int](idx), f(m.col(idx)))
        idx += 1
      end while
    end mapColsInPlace

    inline def mapCols[B](
        inline f: NArray[A] => NArray[B]
    )(using ClassTag[B], ClassTag[A]): Matrix[B] =
      import vecxt.BoundsCheck.DoBoundsCheck.no
      val newArr = NArray.ofSize[B](m.numel)
      // println(m.printMat)
      val m2 = Matrix(newArr, m.rows, m.cols)
      var idx = 0
      while idx < m.cols do
        // println(s"mapCols idx: $idx")
        // println(s"m.col(m): ${m.col(idx).mkString(" ")}, ${f(m.col(idx)).mkString(" ")}")
        m2.updateInPlace(::, NArray[Int](idx), f(m.col(idx)))
        idx += 1
      end while
      m2
    end mapCols

    inline def mapColsToScalar[B](
        inline f: NArray[A] => B
    )(using ClassTag[B], ClassTag[A])(using inline boundsCheck: BoundsCheck): Matrix[B] =
      val newArr = NArray.ofSize[B](m.cols)
      var i = 0
      while i < m.cols do
        newArr(i) = f(m.col(i))
        i += 1
      end while
      Matrix[B](newArr, (1, m.cols))
    end mapColsToScalar

    inline def transpose: Matrix[A] = Matrix(
      raw = m.raw,
      rows = m.cols, // swap dimensions
      cols = m.rows,
      rowStride = m.colStride, // swap strides
      colStride = m.rowStride,
      offset = m.offset // same offset
    )(using BoundsCheck.DoBoundsCheck.no)

    inline def diag(using ClassTag[A]): NArray[A] =
      val minDim = Math.min(m.rows, m.cols)
      val newArr = NArray.ofSize[A](minDim)
      var i = 0
      while i < minDim do
        newArr(i) = m((i, i))(using vecxt.BoundsCheck.DoBoundsCheck.no)
        i += 1
      end while
      newArr
    end diag

    inline def diag(col: Col, startFrom: Vertical, direction: Horizontal)(using ClassTag[A]): NArray[A] =
      val minDim = direction match
        case Horizontal.Right => Math.min(m.rows, m.cols - col)
        case Horizontal.Left  => Math.min(m.rows, col + 1)

      val newArr = NArray.ofSize[A](minDim)
      var i = 0
      while i < minDim do
        val thisRow = if startFrom == Vertical.Top then i else m.rows - i - 1
        val colIdx = if direction == Horizontal.Left then col - i else col + i

        newArr(i) = m((thisRow, colIdx))(using vecxt.BoundsCheck.DoBoundsCheck.no)
        i += 1
      end while
      newArr
    end diag

    inline def diag(row: Row, startFrom: Horizontal, direction: Vertical)(using ClassTag[A]): NArray[A] =
      val minDim = direction match
        case Vertical.Top    => Math.min(m.cols, row + 1)
        case Vertical.Bottom => Math.min(m.rows - row, m.cols)
      val newArr = NArray.ofSize[A](minDim)
      var i = 0
      while i < minDim do
        val thisCol = if startFrom == Horizontal.Right then m.cols - i - 1 else i
        val rowIdx = if direction == Vertical.Bottom then i else row - i
        newArr(i) = m((rowIdx, thisCol))(using vecxt.BoundsCheck.DoBoundsCheck.no)
        i += 1
      end while
      newArr
    end diag

    /** Returns a row of the matrix as an NArray.
      *
      * Note that this copies the data. m.submatrix(i, ::) returns a zero copy view.
      *
      * @param i
      * @return
      */
    inline def row(i: Int)(using ClassTag[A]): NArray[A] =
      val newArr = NArray.ofSize[A](m.cols)
      var j = 0
      while j < m.cols do
        newArr(j) = m((i, j))(using vecxt.BoundsCheck.DoBoundsCheck.no)
        j += 1
      end while
      newArr
    end row

    inline def printMat(using ClassTag[A]): String =
      import vecxt.BoundsCheck.DoBoundsCheck.no
      val arrArr =
        for i <- 0 until m.rows
        yield
          val els =
            for j <- 0 until m.cols
            yield m((i, j)).toString()
          els.mkString(" ")
      arrArr.mkString("\n")
    end printMat

    /** Note that m.submatrix(::, i) will give back a zero-copy matrix with the correct strides.
      *
      * It is probably more efficient
      *
      * @param i
      * @return
      */
    inline def col(i: Int)(using ClassTag[A]): NArray[A] =
      val newArr = NArray.ofSize[A](m.rows)
      var j = 0
      while j < m.rows do
        newArr(j) = m((j, i))(using vecxt.BoundsCheck.DoBoundsCheck.no)
        j += 1
      end while
      newArr

    end col

    inline def horzcat(m2: Matrix[A])(using inline boundsCheck: BoundsCheck, ct: ClassTag[A]): Matrix[A] =
      if m.isDenseColMajor && m2.isDenseColMajor then
        val newShape = (m.rows, m.cols + m2.cols)
        val newArr = m.raw.appendedAll[A](m2.raw)
        Matrix(newArr, newShape)
      else ???
    end horzcat

    inline def vertcat(m2: Matrix[A])(using inline boundsCheck: BoundsCheck, ct: ClassTag[A]): Matrix[A] =
      if m.isDenseColMajor && m2.isDenseColMajor then
        val newShape = (m.rows + m2.rows, m.cols)
        val newArr: NArray[A] = NArray.ofSize[A](newShape._1 * newShape._2)

        var i = 0
        while i < m.cols do
          val column = m.col(i)
          val column2 = m2.col(i)
          column.copyToNArray[A](newArr, i * newShape._1)
          column2.copyToNArray[A](newArr, i * newShape._1 + m.rows)
          i += 1
        end while

        Matrix(newArr, newShape)
      else ???
    end vertcat

  end extension

end matrixUtil
