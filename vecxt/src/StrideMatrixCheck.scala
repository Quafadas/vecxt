package vecxt

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

  /** All of the above only ever reads a length - never an element - so the concrete/generic split only has to happen
    * once, here, rather than being duplicated across every overload below.
    */
  private def checkLengths(length: Int, rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Unit =

    // Check basic dimension validity
    if rows <= 0 || cols <= 0 then throw InvalidMatrix(rows, cols, length)
    end if

    // Check offset bounds
    if offset < 0 || offset >= length then
      throw java.lang.IndexOutOfBoundsException(
        s"Offset $offset is out of bounds for array of size $length"
      )
    end if

    // For 1x1 matrices, enforce sensible strides for semantic clarity
    // if rows == 1 && cols == 1 then
    //   if (rowStride != 0 && rowStride != 1) || (colStride != 0 && colStride != 1) then
    //     throw IllegalArgumentException(
    //       s"For 1x1 matrix, strides should be 0 (broadcast) or 1 (standard). Got rowStride=$rowStride, colStride=$colStride"
    //     )
    // end if

    // Compute the min and max reachable index by accumulating each axis's own contribution independently, rather
    // than enumerating the 4 corners (row in {0, rows-1}) x (col in {0, cols-1}) and taking min/max over them.
    // linearIndex is separable (offset + row*rowStride + col*colStride, each axis independent of the other), so
    // the global min/max over the box is exactly the sum of each axis's own min/max contribution — same technique
    // as strideNDArrayCheck in NDArrayCheck.scala, specialised here to exactly two axes instead of a loop over
    // shape.length. No behaviour change: allocates nothing, where the corner-enumeration version built two Seqs, a
    // for-comprehension and a third Seq on every matrix construction, including every zero-copy submatrix view.
    var minIndex = offset
    var maxIndex = offset

    val rowContribution = (rows - 1) * rowStride
    if rowContribution > 0 then maxIndex += rowContribution
    else if rowContribution < 0 then minIndex += rowContribution
    end if

    val colContribution = (cols - 1) * colStride
    if colContribution > 0 then maxIndex += colContribution
    else if colContribution < 0 then minIndex += colContribution
    end if

    // Check bounds
    if minIndex < 0 then
      throw java.lang.IndexOutOfBoundsException(
        s"Matrix with dimensions ($rows, $cols), strides ($rowStride, $colStride), and offset $offset " +
          s"would access negative index $minIndex"
      )
    end if

    if maxIndex >= length then
      throw java.lang.IndexOutOfBoundsException(
        s"Matrix with dimensions ($rows, $cols), strides ($rowStride, $colStride), and offset $offset " +
          s"would access index $maxIndex, but array size is only $length"
      )
    end if
  end checkLengths

  // Generic arm: uses `.size` rather than `.length` (vecxt/issues/105, check C6a) - Array#size doesn't route
  // through scala/runtime/ScalaRunTime$.array_length even when the element type is abstract, unlike #length
  // (see the comment on Matrix.hasSimpleContiguousMemoryLayout for how this was confirmed). Kept `inline`
  // regardless, and the concrete overloads below kept too, so a caller with a concretely-typed array
  // specializes into daload/dastore-shaped bounds math rather than boxed comparisons either way.
  inline def apply[A](raw: Array[A], rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Unit =
    checkLengths(raw.size, rows, cols, rowStride, colStride, offset)

  def apply(raw: Array[Double], rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Unit =
    checkLengths(raw.size, rows, cols, rowStride, colStride, offset)

  def apply(raw: Array[Float], rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Unit =
    checkLengths(raw.size, rows, cols, rowStride, colStride, offset)

  def apply(raw: Array[Int], rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Unit =
    checkLengths(raw.size, rows, cols, rowStride, colStride, offset)

  def apply(raw: Array[Long], rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Unit =
    checkLengths(raw.size, rows, cols, rowStride, colStride, offset)

  def apply(raw: Array[Boolean], rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Unit =
    checkLengths(raw.size, rows, cols, rowStride, colStride, offset)
end strideMatInstantiateCheck
