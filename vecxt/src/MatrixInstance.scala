package vecxt

import narr.*
import BoundsCheck.BoundsCheck

import rangeExtender.*

import scala.annotation.targetName

import scala.reflect.ClassTag

import matrix.*
import MatrixHelper.zeros
import all.printMat
import vecxt.IntArrays.contiguous

object MatrixInstance:
  extension [@specialized(Double, Boolean, Int) A](m: Matrix[A])
    inline def update(rc: RowCol, value: A)(using inline boundsCheck: BoundsCheck): Unit =
      update(rc._1, rc._2, value)
    end update

    inline def update(row: Row, col: Col, value: A)(using inline boundsCheck: BoundsCheck): Unit =
      indexCheckMat(m, (row, col))
      if m.offset == 0 && m.rowStride == 1 && m.colStride == m.rows then m.raw(col * m.rows + row) = value
      else if m.offset == 0 && m.rowStride == m.cols && m.colStride == 1 then
        // Fast path for default row-major layout (contiguous, no offset/stride)
        m.raw(row * m.cols + col) = value
      else
        // General case: arbitrary offset/stride
        m.raw(m.offset + row * m.rowStride + col * m.colStride) = value
      end if
    end update

    @targetName("updateIdx")
    inline def update(idx: Matrix[Boolean], value: A)(using inline boundsCheck: BoundsCheck): Unit =
      sameDimMatCheck(idx, m)
      var i = 0
      while i < m.numel do
        if idx.raw(i) then m.raw(i) = value
        end if
        i += 1
      end while
    end update

    @targetName("updateFct")
    inline def update(inline fct: A => Boolean, value: A)(using inline boundsCheck: BoundsCheck): Unit =
      var i = 0
      while i < m.rows do
        var j = 0
        while j < m.cols do
          if fct(m(i, j)) then m(i, j) = value
          end if
          j += 1
        end while
        i += 1
      end while
    end update

    inline def updateInPlace(
        row: RangeExtender,
        col: RangeExtender,
        to: NArray[A]
    )(using inline boundsCheck: BoundsCheck): Unit =
      // dimCheckLen(to, m.cols)
      // println("Updating matrix with row: " + row + ", col: " + col)
      // println(to.mkString(", "))
      // println("---")
      val cols = range(col, m.cols)
      val rows = range(row, m.rows)
      (row, col) match
        case (_: ::.type, _) if cols.length == 1 =>

          (0 until m.rows).foreach { i =>
            // println(s"Updating column $i")
            val idx = cols.head * m.rows + i
            m.raw(idx) = to(i)
          }

        case (_, _: ::.type) if rows.length == 1 =>
          (0 until m.cols).foreach { c =>
            // println(s"Updating row $c at idx: ${c * m.rows + r}")
            val idx = c * m.rows + rows.head
            m.raw(idx) = to(c)
          }

        case _ =>
          throw new UnsupportedOperationException(
            "Currently only allowed to update a single row or column. Use (0,::) to update the first row"
          )
      end match
    end updateInPlace

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

    /** Element retrieval
      */
    transparent inline def apply(b: RowCol)(using inline boundsCheck: BoundsCheck): A =
      indexCheckMat(m, b)
      apply(b._1, b._2)
    end apply

    transparent inline def apply(row: Row, col: Col)(using inline boundsCheck: BoundsCheck): A =
      indexCheckMat(m, (row, col))
      // Fast path for default column-major layout (contiguous, no offset/stride)
      if m.offset == 0 && m.rowStride == 1 && m.colStride == m.rows then m.raw(col * m.rows + row)
      else if m.offset == 0 && m.rowStride == m.cols && m.colStride == 1 then
        // Fast path for default row-major layout (contiguous, no offset/stride)
        m.raw(row * m.cols + col)
      else
        // General case: arbitrary offset/stride
        m.raw(m.offset + row * m.rowStride + col * m.colStride)
      end if
    end apply

    /** Returns a matrix of the same dimension, all elements are zero except those selected by the index
      *
      * @param indexes
      * @param boundsCheck
      * @param ct
      * @return
      */
    inline def apply(
        indexes: NArray[RowCol]
    )(using inline boundsCheck: BoundsCheck, ct: ClassTag[A], onz: OneAndZero[A]): Matrix[A] =
      val newMat = Matrix.zeros(m.shape)
      var i = 0
      while i < indexes.length do
        val nextEntry = m(indexes(i))
        newMat(indexes(i)) = nextEntry
        i += 1
      end while
      newMat
    end apply

    def submatrix(rowRange: RangeExtender, colRange: RangeExtender)(using ct: ClassTag[A]): Matrix[A] =
      import BoundsCheck.DoBoundsCheck.no

      val newRows = range(rowRange, m.rows)
      val newCols = range(colRange, m.cols)

      if newRows.contiguous && newCols.contiguous then
        // If rows and cols are contiguous, we can have a zero copy sub-matrix
        val newRowsSpan = newRows.last - newRows.head + 1
        val newColsSpan = newCols.last - newCols.head + 1

        val newOffset = m.offset + newRows.head * m.rowStride + newCols.head * m.colStride

        Matrix(
          raw = m.raw,
          rows = newRowsSpan,
          cols = newColsSpan,
          rowStride = m.rowStride,
          colStride = m.colStride,
          offset = newOffset
        )
      else
        // otherwise, all bets are off...
        val raw = NArray.ofSize[A](newCols.length * newRows.length)
        val newMat = Matrix(raw, newRows.length, newCols.length)
        val mRaw = m.raw
        var i = 0
        while i < newRows.length do
          var j = 0
          while j < newCols.length do
            // println(s"Copying element (${newRows(i)}, ${newCols(j)}) : ${m(newRows(i), newCols(j))}")
            // println(s"into new matrix at ($i, $j})")
            newMat(i, j) = m(newRows(i), newCols(j))
            j += 1
          end while
          i += 1
        end while
        newMat
      end if

    end submatrix

  end extension

end MatrixInstance
