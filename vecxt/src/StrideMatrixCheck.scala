package vecxt

import vecxt.BoundsCheck.BoundsCheck

import narr.*

/** strideMatInstantiateCheck performs a set of safety checks when constructing a matrix view with arbitrary strides and
  * offset into a backing array. The checks include:
  *   - Ensuring the number of rows and columns are positive.
  *   - Ensuring the offset is within the bounds of the backing array.
  *   - Ensuring both rowStride and colStride are non-zero.
  *   - Calculating the maximum and minimum indices that could be accessed by the matrix view, given the strides and
  *     offset, and ensuring these indices are within the bounds of the array.
  *   - Throws appropriate exceptions if any check fails.
  *
  * Validates matrix construction parameters for stride-based layout.
  *
  * Performs comprehensive bounds checking for matrices with flexible stride patterns, including support for
  * broadcasting (zero strides) and negative strides for flipped views.
  *
  * Validates:
  *   - Positive matrix dimensions
  *   - Valid offset within array bounds
  *   - Sensible stride values (zero for broadcasting, non-zero otherwise)
  *   - All matrix elements remain within array bounds
  *   - Negative strides don't cause negative index access
  *   - 1x1 matrices have semantically meaningful strides (0 or 1)
  *
  * @param raw
  *   The underlying data array
  * @param rows
  *   Number of matrix rows
  * @param cols
  *   Number of matrix columns
  * @param rowStride
  *   Memory offset between consecutive rows
  * @param colStride
  *   Memory offset between consecutive columns
  * @param offset
  *   Starting position in the data array
  */
object strideMatInstantiateCheck:
  inline def apply[A](
      raw: NArray[A],
      rows: Row,
      cols: Col,
      rowStride: Int,
      colStride: Int,
      offset: Int
  )(using inline doCheck: BoundsCheck) =
    inline if doCheck then
      // Check basic dimension validity
      if rows <= 0 || cols <= 0 then throw InvalidMatrix(rows, cols, raw.size)
      end if

      // Check offset bounds
      if offset < 0 || offset >= raw.size then
        throw java.lang.IndexOutOfBoundsException(
          s"Offset $offset is out of bounds for array of size ${raw.size}"
        )
      end if

      // For 1x1 matrices, enforce sensible strides for semantic clarity
      if rows == 1 && cols == 1 then
        if (rowStride != 0 && rowStride != 1) || (colStride != 0 && colStride != 1) then
          throw IllegalArgumentException(
            s"For 1x1 matrix, strides should be 0 (broadcast) or 1 (standard). Got rowStride=$rowStride, colStride=$colStride"
          )
      end if

      // Calculate all possible indices that could be accessed
      // For each dimension, we need to consider both i=0 and i=max positions
      val rowIndices =
        if rows > 1 && rowStride != 0 then Seq(0 * rowStride, (rows - 1) * rowStride)
        else Seq(0)

      val colIndices =
        if cols > 1 && colStride != 0 then Seq(0 * colStride, (cols - 1) * colStride)
        else Seq(0)

      // Generate all combinations of row and column offsets
      val allIndices = for
        rowOffset <- rowIndices
        colOffset <- colIndices
      yield offset + rowOffset + colOffset

      val minIndex = allIndices.min
      val maxIndex = allIndices.max

      // Check bounds
      if minIndex < 0 then
        throw java.lang.IndexOutOfBoundsException(
          s"Matrix with dimensions ($rows, $cols), strides ($rowStride, $colStride), and offset $offset " +
            s"would access negative index $minIndex"
        )
      end if

      if maxIndex >= raw.size then
        throw java.lang.IndexOutOfBoundsException(
          s"Matrix with dimensions ($rows, $cols), strides ($rowStride, $colStride), and offset $offset " +
            s"would access index $maxIndex, but array size is only ${raw.size}"
        )
      end if
end strideMatInstantiateCheck
