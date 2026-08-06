package vecxt

import scala.annotation.targetName
import scala.reflect.ClassTag

import vecxt.IntArraysX.contiguous
import vecxt.MatrixHelper.zeros
import vecxt.matrix.*
import vecxt.rangeExtender.*

object MatrixInstance:
  extension [A](m: Matrix[A])
    inline def update(rc: RowCol, value: A): Unit =
      update(rc._1, rc._2, value)
    end update

    /** The two branches this used to have — one for `offset == 0 && rowStride == 1 && colStride == rows`, one for
      * `offset == 0 && rowStride == cols && colStride == 1` — computed exactly what `linearIndex` computes under each
      * of those same guards (`0 + row*1 + col*rows` and `0 + row*cols + col*1` respectively, i.e. the same integer
      * `linearIndex` returns). Unlike the SIMD fast paths in e.g. `doublematrix.scala`, which trade a guard for a
      * genuinely cheaper bulk vectorised op, this guard bought nothing: `linearIndex` is `@Thin` (three field reads and
      * some arithmetic, asserted under `MaxInlineSize` by check C3), so it was already going to inline at this call
      * site for free. The two branches were strictly more bytecode — three comparisons and a duplicated arithmetic
      * expression — for the same result.
      */
    inline def update(row: Row, col: Col, value: A): Unit =
      indexCheckMat(m, (row, col))
      m.raw(m.layout.linearIndex(row, col)) = value
    end update

    /** Sets every element of `m` selected by `idx` to `value`.
      *
      * `idx` and `m` need only agree on shape, not on physical layout — a mask built from a row-major matrix applied to
      * a column-major target (or vice versa) is ordinary usage, since e.g. scalar arithmetic can return either layout
      * depending on its input (see `DoubleMatrix.*`). Reading `idx.raw(i)`/writing `m.raw(i)` at the same flat index
      * `i` is therefore only valid when both share one element order *and* both own their entire backing array
      * (`hasSimpleContiguousMemoryLayout`) — a view's `raw.length` can exceed its `numel`, and its `offset` is never
      * zero-safe to assume away. The fast path below is gated on exactly that; everything else goes through the same
      * `(i, j)` accessors as any other correct-by-construction indexing in this file.
      */
    @targetName("updateIdx")
    inline def update(idx: Matrix[Boolean], value: A): Unit =
      sameDimMatCheck(idx, m)
      val fastPath =
        m.layout.sameElementOrderAs(
          idx.layout
        ) && m.hasSimpleContiguousMemoryLayout && idx.hasSimpleContiguousMemoryLayout
      if fastPath then
        var i = 0
        val bound = m.numel
        while i < bound do
          if idx.raw(i) then m.raw(i) = value
          end if
          i += 1
        end while
      else
        m.layout.foreach2D { (i, j) =>
          if idx(i, j) then m(i, j) = value
          end if
        }
      end if
    end update

    @targetName("updateFct")
    inline def update(inline fct: A => Boolean, value: A): Unit =
      m.layout.foreach2D { (i, j) =>
        if fct(m(i, j)) then m(i, j) = value
        end if
      }
    end update

    /** Overwrites a single row or column of `m` from `to`, in place.
      *
      * Indexes via `m.layout.linearIndex`, so this is correct for any offset/stride — a submatrix view included — not
      * just a dense column-major matrix. Uses a `while` loop rather than `Range#foreach`, since this is an
      * `inline def`: a `for`/`foreach` body is materialised (the `Range` object and the closure) at every call site,
      * not just once.
      */
    inline def updateInPlace(
        row: RangeExtender,
        col: RangeExtender,
        to: Array[A]
    ): Unit =
      val cols = range(col, m.cols)
      val rows = range(row, m.rows)
      (row, col) match
        case (_: ::.type, _) if cols.length == 1 =>
          var i = 0
          while i < m.rows do
            m.raw(m.layout.linearIndex(i, cols.head)) = to(i)
            i += 1
          end while

        case (_, _: ::.type) if rows.length == 1 =>
          var c = 0
          while c < m.cols do
            m.raw(m.layout.linearIndex(rows.head, c)) = to(c)
            c += 1
          end while

        case _ =>
          throw new UnsupportedOperationException(
            "Currently only allowed to update a single row or column. Use (0,::) to update the first row"
          )
      end match
    end updateInPlace

    inline def apply(rowRange: RangeExtender, col: Int)(using ClassTag[A]): Matrix[A] =
      apply(rowRange, Array(col))
    end apply

    inline def apply(row: Int, colRange: RangeExtender)(using ClassTag[A]): Matrix[A] =
      apply(Array(row), colRange)
    end apply

    inline def apply(rowRange: RangeExtender, colRange: RangeExtender)(using ClassTag[A]): Matrix[A] =
      val newRows = range(rowRange, m.rows)
      val newCols = range(colRange, m.cols)
      val newArr = Array.ofDim[A](newCols.size * newRows.size)

      if newRows.contiguous && newCols.contiguous then submatrix(newRows, newCols)
      else if m.isDenseColMajor then
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
        Matrix(newArr, (newRows.size, newCols.size))
      else ???
      end if

    end apply

    /** Returns a deep copy of the matrix. Copies elements one by one.
      *
      * @param ct
      * @return
      */
    inline def deepCopy(using ct: ClassTag[A]): Matrix[A] =
      deepCopy(asRowMajor = false)
    end deepCopy

    /** Returns a deep copy of the matrix with specified layout. Copies elements one by one.
      *
      * @param asRowMajor
      *   If true, returns row-major layout; if false, returns column-major layout
      * @param ct
      * @return
      */
    inline def deepCopy(asRowMajor: Boolean)(using ct: ClassTag[A]): Matrix[A] =
      // println(s"Deep copying matrix with shape ${m.shape} and offset ${m.offset}")
      val newRaw = Array.ofDim[A](m.numel)
      val newMat =
        if asRowMajor then Matrix(newRaw, m.rows, m.cols, m.cols, 1, 0) // row-major: rowStride = cols, colStride = 1
        else Matrix(newRaw, m.rows, m.cols, 1, m.rows, 0) // column-major: rowStride = 1, colStride = rows
      var i = 0
      for row <- 0 until m.rows do
        for col <- 0 until m.cols do
          // println(s"Copying element ($row, $col) with value ${m(row, col)}")
          newMat(row, col) = m(row, col)
          i += 1
        end for
      end for
      newMat
    end deepCopy

    /** Element retrieval
      */
    transparent inline def apply(b: RowCol): A =
      indexCheckMat(m, b)
      apply(b._1, b._2)
    end apply

    def elementIndex(row: Row, col: Col): Int =
      indexCheckMat(m, (row, col))
      // inline if boundsCheck == BoundsCheck.DoBoundsCheck.yes then
      // println(s"Element index for ($row, $col) in matrix with shape ${m.shape} is being checked")
      // println(s"Offset: ${m.offset}, Row stride: ${m.rowStride}, Col stride: ${m.colStride}")
      // println(s"Calculated index: ${m.layout.linearIndex(row, col)}")
      // println(s"element: ${m.raw(m.layout.linearIndex(row, col))}")
      // end if
      m.layout.linearIndex(row, col)

    end elementIndex

    /** See `update(row, col, value)` above: the two layout-specific branches this used to have were provably dead —
      * arithmetically identical, under their own guards, to `linearIndex(row, col)` — and `linearIndex` being `@Thin`
      * means the guards weren't buying a cheaper op either. Since this is `transparent inline`, every one of those
      * redundant comparisons was replicated into every call site in the library and in every downstream user, not paid
      * once.
      */
    transparent inline def apply(row: Row, col: Col): A =
      indexCheckMat(m, (row, col))
      m.raw(m.layout.linearIndex(row, col))
    end apply

    /** Returns a matrix of the same dimension, all elements are zero except those selected by the index
      *
      * @param indexes
      * @param boundsCheck
      * @param ct
      * @return
      */
    inline def apply(
        indexes: Array[RowCol]
    )(using ct: ClassTag[A], onz: OneAndZero[A]): Matrix[A] =
      val newMat = Matrix.zeros(m.shape)
      var i = 0
      while i < indexes.length do
        val nextEntry = m(indexes(i))
        newMat(indexes(i)) = nextEntry
        i += 1
      end while
      newMat
    end apply

    inline def submatrix(rowRange: RangeExtender, colRange: RangeExtender)(using ct: ClassTag[A]): Matrix[A] =

      val newRows = range(rowRange, m.rows)
      val newCols = range(colRange, m.cols)

      if newRows.contiguous && newCols.contiguous then
        // If rows and cols are contiguous, we can have a zero copy sub-matrix
        val newRowsSpan = newRows.last - newRows.head + 1
        val newColsSpan = newCols.last - newCols.head + 1

        val newOffset = m.layout.linearIndex(newRows.head, newCols.head)
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
        val raw = Array.ofDim[A](newCols.length * newRows.length)
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
