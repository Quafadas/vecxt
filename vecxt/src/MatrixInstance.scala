package vecxt

import narr.*
import BoundsCheck.BoundsCheck

import rangeExtender.*

import scala.annotation.targetName

import scala.reflect.ClassTag

import matrix.*
import MatrixHelper.zeros
import all.printMat

object MatrixInstance:
  extension [@specialized(Double, Boolean, Int) A](m: Matrix[A])
    inline def update(rc: RowCol, value: A)(using inline boundsCheck: BoundsCheck): Unit =
      indexCheckMat(m, (rc._1, rc._2): RowCol)
      val idx = rc._2 * m.rows + rc._1
      m.raw(idx) = value
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
    inline def update(inline fct: A => Boolean, value: A) =
      var i = 0
      while i < m.numel do
        if fct(m.raw(i)) then m.raw(i) = value
        end if
        i += 1
      end while
    end update

    def update(
        row: RangeExtender,
        col: RangeExtender,
        to: NArray[A]
    ): Unit = // (using inline boundsCheck: BoundsCheck) =
      // dimCheckLen(to, m.cols)
      // println("Updating matrix with row: " + row + ", col: " + col)
      // println(to.mkString(", "))
      // println("---")
      (row, col) match
        case (_: ::.type, c: Int) =>

          (0 until m.rows).foreach { i =>
            // println(s"Updating column $i")
            val idx = c * m.rows + i
            m.raw(idx) = to(i)
          }

        case (r: Int, _: ::.type) =>
          (0 until m.cols).foreach { c =>
            // println(s"Updating row $c at idx: ${c * m.rows + r}")
            val idx = c * m.rows + r
            m.raw(idx) = to(c)
          }

        case _ =>
          throw new UnsupportedOperationException(
            "Currently only allowed to update a single row or column. Use (0,::) to update the first row"
          )
      end match
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
    inline def apply(b: RowCol)(using inline boundsCheck: BoundsCheck): A =
      indexCheckMat(m, b)
      val idx = b._2 * m.rows + b._1
      m.raw(idx)
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

  end extension

end MatrixInstance
