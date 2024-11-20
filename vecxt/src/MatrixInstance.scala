package vecxt

import scala.reflect.ClassTag

import vecxt.BoundsCheck.BoundsCheck
import vecxt.matrix.*
import vecxt.rangeExtender.*

import narr.*

object MatrixInstance:
  extension [A](m: Matrix[A])

    inline def numel: Int = m.raw.length

    /** element update
      */
    inline def update(loc: RowCol, value: A)(using inline boundsCheck: BoundsCheck) =
      indexCheckMat(m, loc)
      val idx = loc._2 * m.rows + loc._1
      m.raw(idx) = value
    end update

    def apply(rowRange: RangeExtender, colRange: RangeExtender)(using ClassTag[A]): Matrix[A] =
      val newRows = range(rowRange, m.rows)
      val newCols = range(colRange, m.cols)
      val newArr = NArray.ofSize[A](newCols.size * newRows.size)

      var idx = 0

      var i = 0
      while i < newCols.length do
        val colpos = newCols(i)
        val stride = colpos * m.rows
        var j = 0
        while j < newRows.length do
          val rowPos = newRows(j)
          newArr(idx) = m.raw(stride + rowPos)
          idx += 1
          j += 1
        end while
        i += 1
      end while

      Matrix(newArr, (newRows.size, newCols.size))(using BoundsCheck.DoBoundsCheck.no)

    end apply

    /** element retrieval
      */
    inline def apply(b: RowCol)(using inline boundsCheck: BoundsCheck) =
      indexCheckMat(m, b)
      val idx = b._2 * m.rows + b._1
      m.raw(idx)
    end apply

    inline def rows: Row = m.shape._1

    inline def cols: Col = m.shape._2
  end extension

end MatrixInstance
