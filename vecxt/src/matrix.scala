package vecxt
import vecxt.BoundsCheck.BoundsCheck

import narr.*
import scala.annotation.publicInBinary

object matrix:

  /** This is a matrix. The constructor is private to ensure that you deliberately opt in or out of the bounds check.
    *
    * @param raw
    *   The underlying array that holds the matrix data.
    * @param rows
    *   The number of rows in the matrix.
    * @param cols
    *   The number of columns in the matrix.
    * @param rowStride
    *   The stride for rows, used for efficient access.
    * @param colStride
    *   The stride for columns, used for efficient access.
    * @param offset
    *   The offset in the raw array where the matrix data starts.
    * @tparam A
    *   The type of elements in the matrix, specialized for Double, Boolean
    */

  class Matrix[@specialized(Double, Boolean, Int) A] @publicInBinary() private[matrix] (
      val raw: NArray[A],
      val rows: Row,
      val cols: Col,
      val rowStride: Int,
      val colStride: Int,
      val offset: Int = 0
  ):

    /** If the matrix is dense and contiguous, it means that the data is stored in a single block of memory in row or
      * column major, or row major order.
      *
      * We can take advantage of this for performance.
      *
      * @return
      */
    lazy val hasSimpleContiguousMemoryLayout: Boolean =
      isDenseRowMajor || isDenseColMajor

    /** If the matrix is dense and contiguous in row major order, it means that the data is stored in a single block of
      * memory in row major order. Useful for performance optimizations.
      * @return
      */
    lazy val isDenseRowMajor: Boolean =
      rowStride == 1 && colStride == rows && offset == 0

    lazy val isDenseColMajor: Boolean =
      rowStride == cols && colStride == 1 && offset == 0

    lazy val numel = rows * cols
  end Matrix

  object Matrix:

    inline def apply[@specialized(Double, Boolean, Int) A](
        raw: NArray[A],
        rows: Row,
        cols: Col,
        rowStride: Int,
        colStride: Int,
        offset: Int = 0
    )(using inline boundsCheck: BoundsCheck): Matrix[A] =
      strideMatInstantiateCheck(raw, rows, cols, rowStride, colStride, offset)
      new Matrix(
        raw = raw,
        rows = rows,
        cols = cols,
        rowStride = rowStride,
        colStride = colStride,
        offset = offset
      )
    end apply

    inline def apply[@specialized(Double, Boolean, Int) A](raw: NArray[A], dim: RowCol)(using
        inline boundsCheck: BoundsCheck
    ): Matrix[A] =
      dimMatInstantiateCheck(raw, dim)

      new Matrix(
        raw = raw,
        rows = dim._1,
        cols = dim._2,
        rowStride = 1,
        colStride = dim._1,
        offset = 0
      )
    end apply

    inline def apply[@specialized(Double, Boolean, Int) A](raw: NArray[A], rows: Row, cols: Col)(using
        inline boundsCheck: BoundsCheck
    ): Matrix[A] =
      dimMatInstantiateCheck(raw, (rows, cols))
      new Matrix(
        raw = raw,
        rows = rows,
        cols = cols,
        rowStride = 1,
        colStride = rows,
        offset = 0
      )
    end apply

    inline def apply[@specialized(Double, Boolean, Int) A](dim: RowCol, raw: NArray[A])(using
        inline boundsCheck: BoundsCheck
    ): Matrix[A] =
      dimMatInstantiateCheck(raw, dim)
      new Matrix(
        raw = raw,
        rows = dim._1,
        cols = dim._2,
        rowStride = 1,
        colStride = dim._1,
        offset = 0
      )
    end apply
  end Matrix

  extension [@specialized(Double, Boolean, Int) A](m: Matrix[A])

    // transparent inline def refinedRaw = m.raw

    inline def shape: RowCol = (m.rows, m.cols)

    // inline def rows: Row = m._2

    // inline def cols: Col = m._3

    // inline def numel: Int = m.raw.length

  end extension

end matrix
