package vecxt

import scala.reflect.ClassTag

import vecxt.BoundsCheck.BoundsCheck
import vecxt.MatrixInstance.*
import vecxt.matrix.*

import narr.*
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

    // TODO : probably has horrible performance
    def mapRows(f: NArray[A] => NArray[A])(using ClassTag[A])(using boundsCheck: BoundsCheck): Matrix[A] =
      val newArr = NArray.ofSize[A](m.numel)
      val m2 = Matrix(newArr, m.shape)(using BoundsCheck.DoBoundsCheck.no)
      var idx = 0
      while idx < m.rows do
        m2(idx, ::) = f(m.row(idx))
        idx += 1
      end while
      m2
    end mapRows

    inline def mapRowsToScalar(f: NArray[A] => A)(using ClassTag[A])(using inline boundsCheck: BoundsCheck): Matrix[A] =
      val newArr = NArray.ofSize[A](m.rows)
      var i = 0
      while i < m.rows do
        newArr(i) = f(m.row(i))
        i += 1
      end while
      Matrix(newArr, (m.rows, 1))
    end mapRowsToScalar

    inline def mapCols(f: NArray[A] => NArray[A])(using ClassTag[A])(using inline boundsCheck: BoundsCheck): Matrix[A] =
      val newArr = NArray.ofSize[A](m.numel)
      val m2 = Matrix(newArr, m.shape)(using BoundsCheck.DoBoundsCheck.no)
      var idx = 0
      while idx < m.rows do
        m2(::, idx) = f(m.col(idx))
        idx += 1
      end while
      Matrix(newArr, m.shape)
    end mapCols

    inline def mapColsToScalar(f: NArray[A] => A)(using ClassTag[A])(using inline boundsCheck: BoundsCheck): Matrix[A] =
      val newArr = NArray.ofSize[A](m.cols)
      var i = 0
      while i < m.cols do
        newArr(i) = f(m.col(i))
        i += 1
      end while
      Matrix(newArr, (1, m.cols))
    end mapColsToScalar

    inline def transpose(using ClassTag[A]): Matrix[A] =
      import vecxt.BoundsCheck.DoBoundsCheck.no
      val newArr = NArray.ofSize[A](m.numel)
      val newMat = Matrix(newArr, (m.cols.asInstanceOf[Row], m.rows.asInstanceOf[Col]))
      var idx = 0

      while idx < newMat.numel do
        val (row, col) = m.tupleFromIdx(idx)
        newMat((row, col)) = m((col, row))

        idx += 1
      end while
      newMat
    end transpose

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

    inline def row(i: Int)(using ClassTag[A]): NArray[A] =
      // println(s"row $i")
      val m2 = m(i, ::)
      m2.raw
    end row

    inline def printMat(using ClassTag[A]): String =
      val arrArr =
        for i <- 0 until m.rows
        yield
          val aRow = m.row(i)
          val els =
            for (el <- aRow)
              yield el.toString()
          els.mkString(" ")
      end arrArr
      arrArr.mkString("\n")
    end printMat

    inline def col(i: Int)(using ClassTag[A]): NArray[A] =
      m(::, i).raw
    end col

    inline def horzcat(m2: Matrix[A])(using inline boundsCheck: BoundsCheck, ct: ClassTag[A]): Matrix[A] =
      val newShape = (m.rows, m.cols + m2.cols)
      val newArr = m.raw.appendedAll[A](m2.raw)
      Matrix(newArr, newShape)
    end horzcat

    inline def vertcat(m2: Matrix[A])(using inline boundsCheck: BoundsCheck, ct: ClassTag[A]): Matrix[A] =
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
    end vertcat

  end extension

end matrixUtil
