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

  /** Writes `src` into row `i` of `dest`, for any layout `dest` may have.
    *
    * Extracted because the `mapRows*` / `mapCols*` family all used to write back via
    * `updateInPlace(Array[Int](idx), ::, ...)`, which allocates three arrays per line: the one-element `Array[Int]`
    * naming the line, and then — inside `updateInPlace` — `range(::, n)`, which is `Array.from((0 until n).toArray)`
    * and so builds the full index array *twice*. None of that survives here: the line index stays an `Int`, and the
    * walk is the same hoisted-base strided loop as `row`, degenerating to an `arraycopy` for a contiguous row.
    *
    * Top-level rather than a member of the `Matrix[A]` extension below because `mapRows[B]` writes into a `Matrix[B]`,
    * so the destination's element type is independent of the source's.
    */
  private inline def writeRow[C](dest: Matrix[C], i: Int, src: Array[C]): Unit =
    if src.length != dest.cols then throw MatrixDimensionMismatch(1, dest.cols, 1, src.length)
    end if
    val base = dest.layout.linearIndex(i, 0)
    if dest.colStride == 1 then System.arraycopy(src, 0, dest.raw, base, dest.cols)
    else
      var j = 0
      var idx = base
      while j < dest.cols do
        dest.raw(idx) = src(j)
        idx += dest.colStride
        j += 1
      end while
    end if
  end writeRow

  /** Column-wise mirror of [[writeRow]]. */
  private inline def writeCol[C](dest: Matrix[C], i: Int, src: Array[C]): Unit =
    if src.length != dest.rows then throw MatrixDimensionMismatch(dest.rows, 1, src.length, 1)
    end if
    val base = dest.layout.linearIndex(0, i)
    if dest.rowStride == 1 then System.arraycopy(src, 0, dest.raw, base, dest.rows)
    else
      var j = 0
      var idx = base
      while j < dest.rows do
        dest.raw(idx) = src(j)
        idx += dest.rowStride
        j += 1
      end while
    end if
  end writeCol

  extension [A](m: Matrix[A])

    inline def mapRowsInPlace(
        inline f: Array[A] => Array[A]
    )(using ClassTag[A]): Unit =
      var idx = 0
      while idx < m.rows do
        writeRow(m, idx, f(m.row(idx)))
        idx += 1
      end while
    end mapRowsInPlace

    /** The result is dense **row**-major, not column-major like the rest of the `Matrix(raw, rows, cols)` factories.
      *
      * This is a row-wise operation: it reads a row at a time and writes a row at a time. A column-major destination
      * makes every one of those writes stride by `rows`, whereas a row-major one makes each row a contiguous run — so
      * `writeRow` takes its `arraycopy` path instead of the strided loop, and on a row-major source the whole method
      * becomes `arraycopy` in, `arraycopy` out. It also matches the layout policy the scalar ops already document in
      * `doublematrix.scala` (result orientation follows the operation's own unit-stride axis) rather than
      * unconditionally normalising to column-major.
      *
      * [[mapCols]] deliberately keeps the column-major default for the mirror-image reason.
      *
      * The zero-dimension guard is not cosmetic: the strided factory routes through `strideMatInstantiateCheck`, which
      * rejects `rows <= 0 || cols <= 0`, while the `(raw, rows, cols)` factory permits them. Without it an empty matrix
      * would start throwing `InvalidMatrix` where it previously mapped to an empty result.
      */
    inline def mapRows[B](
        inline f: Array[A] => Array[B]
    )(using ClassTag[B], ClassTag[A]): Matrix[B] =
      val newArr = Array.ofDim[B](m.numel)
      val m2 =
        if m.rows == 0 || m.cols == 0 then Matrix(newArr, m.rows, m.cols)
        else Matrix(newArr, m.rows, m.cols, m.cols, 1, 0)
      var idx = 0
      while idx < m.rows do
        writeRow(m2, idx, f(m.row(idx)))
        idx += 1
      end while
      m2
    end mapRows

    /** The `isDenseRowMajor` special case this used to carry — allocate the row and `System.arraycopy` into it — is now
      * exactly what [[row]] does on its own, under the strictly weaker condition `colStride == 1`. That covers
      * dense row-major plus every row-contiguous layout it excluded (non-zero offset, padded row stride), so the
      * duplicated branch bought nothing and applied less often than the general one it was guarding.
      */
    inline def mapRowsToScalar[B](
        inline f: Array[A] => B
    )(using ClassTag[B], ClassTag[A]): Matrix[B] =
      val newArr = Array.ofDim[B](m.rows)
      var i = 0
      while i < m.rows do
        newArr(i) = f(m.row(i))
        i += 1
      end while
      Matrix(newArr, (m.rows, 1))
    end mapRowsToScalar

    inline def mapColsInPlace(
        inline f: Array[A] => Array[A]
    )(using ClassTag[A]): Unit =
      var idx = 0
      while idx < m.cols do
        writeCol(m, idx, f(m.col(idx)))
        idx += 1
      end while
    end mapColsInPlace

    inline def mapCols[B](
        inline f: Array[A] => Array[B]
    )(using ClassTag[B], ClassTag[A]): Matrix[B] =
      val newArr = Array.ofDim[B](m.numel)
      val m2 = Matrix(newArr, m.rows, m.cols)
      var idx = 0
      while idx < m.cols do
        writeCol(m2, idx, f(m.col(idx)))
        idx += 1
      end while
      m2
    end mapCols

    /** Column-wise mirror of [[mapRowsToScalar]]; its `isDenseColMajor` branch is subsumed by [[col]]'s
      * `rowStride == 1` fast path for the same reason.
      */
    inline def mapColsToScalar[B](
        inline f: Array[A] => B
    )(using ClassTag[B], ClassTag[A]): Matrix[B] =
      val newArr = Array.ofDim[B](m.cols)
      var i = 0
      while i < m.cols do
        newArr(i) = f(m.col(i))
        i += 1
      end while
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
    def transpose: Matrix[A] = Matrix.mkMatrix(m.raw, m.layout.transpose)

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
      * Deliberately *not* written with `layout.foreach2D`: that traverses the whole matrix, so it would turn an
      * `O(cols)` read into an `O(rows * cols)` one. `foreach2D` is the right tool for whole-matrix passes, not for
      * extracting a single line out of one.
      *
      * The walk is a running index rather than `m((i, j))` per element. `linearIndex(i, j)` is
      * `offset + i * rowStride + j * colStride`, in which only the `j * colStride` term varies here — so the row's base
      * is hoisted and each step is one add, instead of a multiply-add plus the bounds check that `apply(RowCol)` runs
      * per element. When the row is contiguous (`colStride == 1`) it degenerates to a straight `arraycopy`.
      *
      * @param i
      * @return
      */
    inline def row(i: Int)(using ClassTag[A]): Array[A] =
      // Validates `i` against `rows` only, rather than `indexCheckMat(m, (i, 0))`: the latter also demands
      // `0 < cols`, which would reject a legitimately empty (zero-column) matrix that the old per-element loop
      // simply returned an empty array for.
      if i < 0 || i >= m.rows then
        throw java.lang.IndexOutOfBoundsException(
          s"Tried to read row $i of a ${m.rows} x ${m.cols} matrix, which is not valid."
        )
      end if
      val newArr = Array.ofDim[A](m.cols)
      val base = m.layout.linearIndex(i, 0)
      if m.colStride == 1 then System.arraycopy(m.raw, base, newArr, 0, m.cols)
      else
        var j = 0
        var idx = base
        while j < m.cols do
          newArr(j) = m.raw(idx)
          idx += m.colStride
          j += 1
        end while
      end if
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
      * Mirror of [[row]] — see there for why this is a strided walk rather than `layout.foreach2D`. Here only the
      * `i * rowStride` term varies, so the column's base is hoisted and each step adds `rowStride`; a contiguous column
      * (`rowStride == 1`, i.e. the column-major case) becomes an `arraycopy`.
      *
      * @param i
      * @return
      */
    inline def col(i: Int)(using ClassTag[A]): Array[A] =
      // Validates `i` against `cols` only — see [[row]].
      if i < 0 || i >= m.cols then
        throw java.lang.IndexOutOfBoundsException(
          s"Tried to read column $i of a ${m.rows} x ${m.cols} matrix, which is not valid."
        )
      end if
      val newArr = Array.ofDim[A](m.rows)
      val base = m.layout.linearIndex(0, i)
      if m.rowStride == 1 then System.arraycopy(m.raw, base, newArr, 0, m.rows)
      else
        var j = 0
        var idx = base
        while j < m.rows do
          newArr(j) = m.raw(idx)
          idx += m.rowStride
          j += 1
        end while
      end if
      newArr
    end col

    /** Concatenates `m2` to the right of `m`, producing a `(m.rows, m.cols + m2.cols)` dense column-major matrix.
      *
      * Works for any layout on either side — strided views, row-major, offset submatrices — because both operands are
      * read through their own `linearIndex`. The `hasSimpleContiguousMemoryLayout && isDenseColMajor` fast path is an
      * optimisation only: under exactly that condition each operand's backing array already *is* its elements in
      * destination order, so the whole thing is two `arraycopy`s. `hasSimpleContiguousMemoryLayout` (not just
      * `isDenseColMajor`) is what makes that sound — it additionally requires `dataLength == numel`, ruling out a
      * dense-by-stride view that still carries a larger parent array behind it.
      *
      * @throws MatrixDimensionMismatch
      *   if the two matrices do not have the same number of rows.
      */
    def horzcat(m2: Matrix[A])(using ct: ClassTag[A]): Matrix[A] =
      if m.rows != m2.rows then throw MatrixDimensionMismatch(m.rows, m.cols, m2.rows, m2.cols)
      end if

      val newRows = m.rows
      val newArr: Array[A] = Array.ofDim[A](newRows * (m.cols + m2.cols))

      if m.hasSimpleContiguousMemoryLayout && m.isDenseColMajor then
        System.arraycopy(m.raw, 0, newArr, 0, m.numel)
      else
        m.layout.foreach2D { (i, j) =>
          newArr(i + j * newRows) = m.raw(m.layout.linearIndex(i, j))
        }
      end if

      if m2.hasSimpleContiguousMemoryLayout && m2.isDenseColMajor then
        System.arraycopy(m2.raw, 0, newArr, m.numel, m2.numel)
      else
        m2.layout.foreach2D { (i, j) =>
          newArr(i + (j + m.cols) * newRows) = m2.raw(m2.layout.linearIndex(i, j))
        }
      end if

      Matrix(newArr, (newRows, m.cols + m2.cols))
    end horzcat

    /** Concatenates `m2` underneath `m`, producing a `(m.rows + m2.rows, m.cols)` dense column-major matrix.
      *
      * Layout-agnostic for the same reason as [[horzcat]]. There is no whole-array fast path here: in column-major
      * order the two operands interleave column by column, so even for two dense column-major inputs the copy is one
      * `arraycopy` per column rather than one per matrix.
      *
      * @throws MatrixDimensionMismatch
      *   if the two matrices do not have the same number of columns.
      */
    def vertcat(m2: Matrix[A])(using ct: ClassTag[A]): Matrix[A] =
      if m.cols != m2.cols then throw MatrixDimensionMismatch(m.rows, m.cols, m2.rows, m2.cols)
      end if

      val newRows = m.rows + m2.rows
      val newArr: Array[A] = Array.ofDim[A](newRows * m.cols)

      if m.hasSimpleContiguousMemoryLayout && m.isDenseColMajor then
        var j = 0
        while j < m.cols do
          System.arraycopy(m.raw, j * m.rows, newArr, j * newRows, m.rows)
          j += 1
        end while
      else
        m.layout.foreach2D { (i, j) =>
          newArr(i + j * newRows) = m.raw(m.layout.linearIndex(i, j))
        }
      end if

      if m2.hasSimpleContiguousMemoryLayout && m2.isDenseColMajor then
        var j = 0
        while j < m2.cols do
          System.arraycopy(m2.raw, j * m2.rows, newArr, j * newRows + m.rows, m2.rows)
          j += 1
        end while
      else
        m2.layout.foreach2D { (i, j) =>
          newArr(m.rows + i + j * newRows) = m2.raw(m2.layout.linearIndex(i, j))
        }
      end if

      Matrix(newArr, (newRows, m.cols))
    end vertcat

  end extension

end matrixUtil
