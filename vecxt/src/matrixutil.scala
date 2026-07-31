package vecxt

import scala.reflect.ClassTag

import vecxt.MatrixInstance.*
import vecxt.matrix.*

// import vecxt.arrayUtil.printArr
object matrixUtil:
  enum Vertical:
    case Top, Bottom
  end Vertical

  enum Horizontal:
    case Left, Right
  end Horizontal

  extension [@specialized(Double, Boolean, Int) A](m: Matrix[A])

    private def tupleFromIdx(b: Int): RowCol =
      // dimCheckLen(m.raw, b)
      (b / m.rows, b % m.rows)
    end tupleFromIdx

    /** There should be
      *
      * @param f
      */
    inline def mapRowsInPlace(
        inline f: Array[A] => Array[A]
    )(using ClassTag[A]): Unit =
      var idx = 0
      while idx < m.rows do
        m.updateInPlace(Array[Int](idx), ::, f(m.row(idx)))
        idx += 1
      end while
    end mapRowsInPlace

    inline def mapRows[B](
        inline f: Array[A] => Array[B]
    )(using ClassTag[B], ClassTag[A]): Matrix[B] =
      val newArr = Array.ofDim[B](m.numel)
      val m2 = Matrix(newArr, m.rows, m.cols)
      var idx = 0
      while idx < m.rows do
        m2.updateInPlace(Array[Int](idx), ::, f(m.row(idx)))
        idx += 1
      end while
      m2
    end mapRows

    inline def mapRowsToScalar[B](
        inline f: Array[A] => B
    )(using ClassTag[B], ClassTag[A]): Matrix[B] =
      val newArr = Array.ofDim[B](m.rows)
      var i = 0
      if m.isDenseRowMajor then
        while i < m.rows do
          val rowArr = Array.ofDim[A](m.cols)
          System.arraycopy(m.raw, i * m.cols, rowArr, 0, m.cols)
          newArr(i) = f(rowArr)
          i += 1
        end while
      else
        while i < m.rows do
          newArr(i) = f(m.row(i))
          i += 1
        end while
      end if
      Matrix(newArr, (m.rows, 1))
    end mapRowsToScalar

    inline def mapColsInPlace(
        inline f: Array[A] => Array[A]
    )(using ClassTag[A]): Unit =

      var idx = 0
      while idx < m.cols do
        m.updateInPlace(::, Array[Int](idx), f(m.col(idx)))
        idx += 1
      end while
    end mapColsInPlace

    inline def mapCols[B](
        inline f: Array[A] => Array[B]
    )(using ClassTag[B], ClassTag[A]): Matrix[B] =
      val newArr = Array.ofDim[B](m.numel)
      // println(m.printMat)
      val m2 = Matrix(newArr, m.rows, m.cols)
      var idx = 0
      while idx < m.cols do
        // println(s"mapCols idx: $idx")
        // println(s"m.col(m): ${m.col(idx).mkString(" ")}, ${f(m.col(idx)).mkString(" ")}")
        m2.updateInPlace(::, Array[Int](idx), f(m.col(idx)))
        idx += 1
      end while
      m2
    end mapCols

    inline def mapColsToScalar[B](
        inline f: Array[A] => B
    )(using ClassTag[B], ClassTag[A]): Matrix[B] =
      val newArr = Array.ofDim[B](m.cols)
      var i = 0
      if m.isDenseColMajor then
        while i < m.cols do
          val colArr = Array.ofDim[A](m.rows)
          System.arraycopy(m.raw, i * m.rows, colArr, 0, m.rows)
          newArr(i) = f(colArr)
          i += 1
        end while
      else
        while i < m.cols do
          newArr(i) = f(m.col(i))
          i += 1
        end while
      end if
      Matrix[B](newArr, (1, m.cols))
    end mapColsToScalar

    /** Alias for transpose
      *
      * @return
      */
    def T: Matrix[A] = m.transpose

    /** Returns the transpose of this matrix by swapping rows and columns.
      *
      * This operation is performed efficiently by swapping the dimensions and strides without copying the underlying
      * data. The transposed matrix shares the same underlying raw data as the original matrix.
      *
      * @return
      *   a new Matrix with rows and columns swapped, sharing the same underlying data
      *
      * @example
      *   {{{
      * val m = Matrix(Array(1, 2, 3, 4), rows = 2, cols = 2)
      * // m = [[1, 2],
      * //      [3, 4]]
      * val mt = m.transpose
      * // mt = [[1, 3],
      * //       [2, 4]]
      *   }}}
      */
    def transpose: Matrix[A] = Matrix(
      raw = m.raw,
      rows = m.cols, // swap dimensions
      cols = m.rows,
      rowStride = m.colStride, // swap strides
      colStride = m.rowStride,
      offset = m.offset // same offset
    )

    inline def diag(using ClassTag[A]): Array[A] =
      val minDim = Math.min(m.rows, m.cols)
      val newArr = Array.ofDim[A](minDim)
      var i = 0
      while i < minDim do
        newArr(i) = m((i, i))
        i += 1
      end while
      newArr
    end diag

    inline def diag(col: Col, startFrom: Vertical, direction: Horizontal)(using ClassTag[A]): Array[A] =
      val minDim = direction match
        case Horizontal.Right => Math.min(m.rows, m.cols - col)
        case Horizontal.Left  => Math.min(m.rows, col + 1)

      val newArr = Array.ofDim[A](minDim)
      var i = 0
      while i < minDim do
        val thisRow = if startFrom == Vertical.Top then i else m.rows - i - 1
        val colIdx = if direction == Horizontal.Left then col - i else col + i

        newArr(i) = m((thisRow, colIdx))
        i += 1
      end while
      newArr
    end diag

    inline def diag(row: Row, startFrom: Horizontal, direction: Vertical)(using ClassTag[A]): Array[A] =
      val minDim = direction match
        case Vertical.Top    => Math.min(m.cols, row + 1)
        case Vertical.Bottom => Math.min(m.rows - row, m.cols)
      val newArr = Array.ofDim[A](minDim)
      var i = 0
      while i < minDim do
        val thisCol = if startFrom == Horizontal.Right then m.cols - i - 1 else i
        val rowIdx = if direction == Vertical.Bottom then i else row - i
        newArr(i) = m((rowIdx, thisCol))
        i += 1
      end while
      newArr
    end diag

    /** Returns a row of the matrix as an Array.
      *
      * Note that this copies the data. m.submatrix(i, ::) returns a zero copy view.
      *
      * @param i
      * @return
      */
    inline def row(i: Int)(using ClassTag[A]): Array[A] =
      val newArr = Array.ofDim[A](m.cols)
      var j = 0
      while j < m.cols do
        newArr(j) = m((i, j))
        j += 1
      end while
      newArr
    end row

    /** Renders the matrix as rows of space-separated elements, one row per line, via `toString` on each element.
      *
      * Debug output, not a serialisation format: no alignment, no truncation of large matrices, and no stable contract
      * on the result. Reads every element generically and boxes as it goes, which is fine here and would not be on a
      * fast path.
      *
      * Appends into a `StringBuilder` rather than building nested collections and joining them, so it creates no
      * intermediate `Seq` per row and no closures.
      */
    def printMat(using ClassTag[A]): String =
      val sb = new StringBuilder
      var i = 0
      while i < m.rows do
        if i > 0 then sb.append('\n')
        end if
        var j = 0
        while j < m.cols do
          if j > 0 then sb.append(' ')
          end if
          sb.append(m((i, j)).toString())
          j += 1
        end while
        i += 1
      end while
      sb.toString
    end printMat

    /** Note that m.submatrix(::, i) will give back a zero-copy matrix with the correct strides.
      *
      * It is probably more efficient
      *
      * @param i
      * @return
      */
    inline def col(i: Int)(using ClassTag[A]): Array[A] =
      val newArr = Array.ofDim[A](m.rows)
      var j = 0
      while j < m.rows do
        newArr(j) = m((j, i))
        j += 1
      end while
      newArr

    end col

    def horzcat(m2: Matrix[A])(using ct: ClassTag[A]): Matrix[A] =
      if m.isDenseColMajor && m2.isDenseColMajor then
        val newShape = (m.rows, m.cols + m2.cols)
        val newArr = m.raw.appendedAll[A](m2.raw)
        Matrix(newArr, newShape)
      else ???
    end horzcat

    inline def vertcat(m2: Matrix[A])(using ct: ClassTag[A]): Matrix[A] =
      if m.isDenseColMajor && m2.isDenseColMajor then
        val newShape = (m.rows + m2.rows, m.cols)
        val newArr: Array[A] = Array.ofDim[A](newShape._1 * newShape._2)

        var i = 0
        while i < m.cols do
          val column = m.col(i)
          val column2 = m2.col(i)
          column.copyToArray[A](newArr, i * newShape._1)
          column2.copyToArray[A](newArr, i * newShape._1 + m.rows)
          i += 1
        end while

        Matrix(newArr, newShape)
      else ???
    end vertcat

  end extension

end matrixUtil
