package vecxt
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

  final class Matrix[A] @publicInBinary() private[matrix] (
      val raw: Array[A],
      val rows: Row,
      val cols: Col,
      val rowStride: Int,
      val colStride: Int,
      val offset: Int = 0
  ):

    /** If the matrix is dense and contiguous in row major order, it means that the data is stored in a single block of
      * memory in row major order. Useful for performance optimizations.
      * @return
      */
    val isDenseColMajor: Boolean =
      rowStride == 1 && colStride == rows && offset == 0

    val isDenseRowMajor: Boolean =
      rowStride == cols && colStride == 1 && offset == 0

    val numel: Int = rows * cols

    /** If the matrix is dense and contiguous, it means that the data is stored in a single block of memory in row or
      * column major, or row major order, with the exact number of elements matching the number of rows and columns.
      *
      * We can take advantage of this for performance.
      *
      * @return
      */
    // `raw.length` here and in `layout` below routes through ScalaRunTime$.array_length (vecxt/issues/105,
    // check C6a) - unlike Matrix.apply's overloads, this can't be fixed by overloading, because there is
    // only one compiled Matrix class no matter how many concrete factory overloads construct it: `A` is
    // this class's own abstract parameter at this point, not a caller's concrete one. `layout` could move
    // to a `Matrix[Double]`-style extension (same trick used elsewhere) with no behavioural change since
    // it's already recomputed on every call, but that removes a public instance method from a published
    // library's binary shape - a call worth making deliberately, not as a side effect of this audit. Both
    // run once per construction/toString, not per element, so the cost here is the label, not the number.
    val hasSimpleContiguousMemoryLayout: Boolean =
      (isDenseRowMajor || isDenseColMajor) && raw.length == numel

    def layout: String =
      s"rows: $rows, cols: $cols, rowStride: $rowStride, colStride: $colStride, offset: $offset, data length: ${raw.length}"
  end Matrix

  object Matrix:

    def apply[A](
        raw: Array[A],
        rows: Row,
        cols: Col,
        rowStride: Int,
        colStride: Int,
        offset: Int = 0
    ): Matrix[A] =
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

    // Concrete overloads of the constructor above (vecxt/issues/105, check C6a): overload resolution
    // picks these over the generic `apply[A]` whenever the caller already holds a concretely-typed
    // array, which routes the call into strideMatInstantiateCheck's matching concrete overload instead
    // of its generic one. This only fixes the check *inside this factory* - Matrix's own constructor
    // body (`hasSimpleContiguousMemoryLayout`) stays generic regardless, since it's the same single
    // compiled class either way; see the comment on that field.
    def apply(raw: Array[Double], rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Matrix[Double] =
      strideMatInstantiateCheck(raw, rows, cols, rowStride, colStride, offset)
      new Matrix(raw = raw, rows = rows, cols = cols, rowStride = rowStride, colStride = colStride, offset = offset)

    def apply(raw: Array[Float], rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Matrix[Float] =
      strideMatInstantiateCheck(raw, rows, cols, rowStride, colStride, offset)
      new Matrix(raw = raw, rows = rows, cols = cols, rowStride = rowStride, colStride = colStride, offset = offset)

    def apply(raw: Array[Int], rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Matrix[Int] =
      strideMatInstantiateCheck(raw, rows, cols, rowStride, colStride, offset)
      new Matrix(raw = raw, rows = rows, cols = cols, rowStride = rowStride, colStride = colStride, offset = offset)

    def apply(raw: Array[Long], rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Matrix[Long] =
      strideMatInstantiateCheck(raw, rows, cols, rowStride, colStride, offset)
      new Matrix(raw = raw, rows = rows, cols = cols, rowStride = rowStride, colStride = colStride, offset = offset)

    def apply(
        raw: Array[Boolean],
        rows: Row,
        cols: Col,
        rowStride: Int,
        colStride: Int,
        offset: Int
    ): Matrix[Boolean] =
      strideMatInstantiateCheck(raw, rows, cols, rowStride, colStride, offset)
      new Matrix(raw = raw, rows = rows, cols = cols, rowStride = rowStride, colStride = colStride, offset = offset)

    def apply[A](raw: Array[A], dim: RowCol): Matrix[A] =
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

    /** Assumes column major order.
      *
      * @param raw
      * @param rows
      * @param cols
      * @param boundsCheck
      * @return
      */
    def apply[A](raw: Array[A], rows: Row, cols: Col): Matrix[A] =
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

    def apply[A](dim: RowCol, raw: Array[A]): Matrix[A] =
      Matrix(raw, dim._1, dim._2)
    end apply
  end Matrix

  extension [A](m: Matrix[A])

    // transparent inline def refinedRaw = m.raw

    def shape: RowCol = (m.rows, m.cols)

    // inline def rows: Row = m._2

    // inline def cols: Col = m._3

    // inline def numel: Int = m.raw.length

  end extension

end matrix
