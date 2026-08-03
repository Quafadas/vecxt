package vecxt
import scala.annotation.publicInBinary

object matrix:

  /** Describes the memory layout of a [[Matrix]]: dimensions, strides, offset, and the length of the backing array.
    *
    * This is a `final class` (not a `case class` and not a `sealed trait`) for performance reasons:
    *   - `case class` synthesises `productElement: Int => Object`, which boxes — unacceptable on the central layout
    *     type of a performance library (vecxt/issues/105).
    *   - A `sealed trait` makes all field reads `invokeinterface`, which defeats C2's escape analysis and blocks scalar
    *     replacement of `FloatVector`/`VectorMask` temporaries.
    *   - `final class` gives 5-byte getters, under `MaxTrivialSize` (6) — the strongest inline category.
    *
    * The `kind` field is reserved for Phase 3 structured layouts (Diagonal, Triangular, etc.). In Phase 1/2 it is
    * always [[Layout.Strided]]. Do not branch on `kind` yet.
    *
    * Invariant enforced by every checked factory: `raw.size == dataLength`. This is what makes a shared `Layout` safe —
    * the array length is baked in, not re-read from the array. The `.size`-not-`.length` note: `dataLength` is set by
    * the factory overloads using `raw.size`, which avoids the ScalaRunTime$.array_length path for abstract element
    * types (vecxt/issues/105, check C6a). The predicate `hasSimpleContiguousMemoryLayout` is now computed entirely from
    * `Layout` fields and is no longer generic.
    */
  final class Layout @publicInBinary() private[matrix] (
      val rows: Row,
      val cols: Col,
      val rowStride: Int,
      val colStride: Int,
      val offset: Int,
      val dataLength: Int,
      val kind: Byte
  ):
    val numel: Int = rows * cols
    val isDenseColMajor: Boolean = rowStride == 1 && colStride == rows && offset == 0
    val isDenseRowMajor: Boolean = rowStride == cols && colStride == 1 && offset == 0
    val hasSimpleContiguousMemoryLayout: Boolean =
      (isDenseRowMajor || isDenseColMajor) && dataLength == numel

    /** Computes the linear (flat) index into the backing array for element at (row, col).
      *
      * Inlined into every call site — the expression expands to ~4 JVM bytecodes. No separate method is emitted.
      */
    inline def linearIndex(row: Int, col: Int): Int = offset + row * rowStride + col * colStride

    /** Returns the transposed layout: rows ↔ cols, rowStride ↔ colStride. Shares the same backing data. */
    def transpose: Layout = new Layout(cols, rows, colStride, rowStride, offset, dataLength, kind)

    /** Returns a copy of this layout with a different `dataLength`. Use when constructing a matrix backed by an array
      * of a different size than the original (e.g., after slicing or materialising a submatrix).
      */
    def withDataLength(newLength: Int): Layout =
      new Layout(rows, cols, rowStride, colStride, offset, newLength, kind)

    /** Returns true if this layout and `that` layout have the same element traversal order, which allows elementwise
      * operations to proceed directly over the raw arrays without reindexing.
      *
      * Semantics are identical to the original `sameDenseElementWiseMemoryLayoutCheck` expression — operator precedence
      * is preserved literally, not "cleaned up".
      */
    def sameElementOrderAs(that: Layout): Boolean =
      isDenseColMajor && that.isDenseColMajor && rowStride == that.rowStride || isDenseRowMajor && that.isDenseRowMajor && colStride == that.colStride

    /** Reproduces the original `Matrix#layout` string byte-for-byte so no test or `experiments` output changes. */
    override def toString: String =
      s"rows: $rows, cols: $cols, rowStride: $rowStride, colStride: $colStride, " +
        s"offset: $offset, data length: $dataLength"

    override def equals(that: Any): Boolean = that match
      case l: Layout =>
        rows == l.rows && cols == l.cols && rowStride == l.rowStride &&
        colStride == l.colStride && offset == l.offset && dataLength == l.dataLength && kind == l.kind
      case _ => false

    override def hashCode: Int =
      var h = rows.hashCode()
      h = 31 * h + cols.hashCode()
      h = 31 * h + rowStride.hashCode()
      h = 31 * h + colStride.hashCode()
      h = 31 * h + offset.hashCode()
      h = 31 * h + dataLength.hashCode()
      h = 31 * h + kind.hashCode()
      h
    end hashCode
  end Layout

  object Layout:
    /** Standard strided layout — the only kind used in Phase 1 and Phase 2. */
    val Strided: Byte = 0

    /** Public factory for creating [[Layout]] instances. The constructor is `private[matrix]` to prevent bypassing the
      * checked `Matrix` factories; this companion factory allows direct construction for testing and internal tooling.
      */
    def apply(
        rows: Row,
        cols: Col,
        rowStride: Int,
        colStride: Int,
        offset: Int,
        dataLength: Int,
        kind: Byte = Strided
    ): Layout =
      new Layout(rows, cols, rowStride, colStride, offset, dataLength, kind)
  end Layout

  /** This is a matrix. The constructor is private to ensure that you deliberately opt in or out of the bounds check.
    *
    * @param raw
    *   The underlying array that holds the matrix data.
    * @param layout
    *   The memory layout describing dimensions, strides, offset and data length.
    * @tparam A
    *   The type of elements in the matrix, specialized for Double, Boolean
    */
  final class Matrix[A] @publicInBinary() private[matrix] (
      val raw: Array[A],
      val layout: Layout
  ):
    inline def rows: Row = layout.rows
    inline def cols: Col = layout.cols
    inline def rowStride: Int = layout.rowStride
    inline def colStride: Int = layout.colStride
    inline def offset: Int = layout.offset
    inline def numel: Int = layout.numel
    inline def isDenseColMajor: Boolean = layout.isDenseColMajor
    inline def isDenseRowMajor: Boolean = layout.isDenseRowMajor

    /** If the matrix is dense and contiguous, it means that the data is stored in a single block of memory in row or
      * column major order, with the exact number of elements matching the number of rows and columns.
      *
      * We can take advantage of this for performance.
      *
      * Originally checked `(isDenseRowMajor || isDenseColMajor) && raw.size == numel`. `raw.size` is now baked into
      * `layout.dataLength` at construction time (vecxt/issues/105, check C6a): every checked factory reads `raw.size`
      * once and stores it in the `Layout`, so this predicate is no longer generic — it reads only `Int` fields.
      */
    inline def hasSimpleContiguousMemoryLayout: Boolean = layout.hasSimpleContiguousMemoryLayout

    /** Runtime element type of the backing store - `double` for a specialised `Matrix[Double]`, `java.lang.Object` for
      * one whose data got boxed into an `Object[]`. See `NDArray#elementClass` for why this is exposed rather than
      * asserted in the constructor, and `specialisation.test.scala` for what asserts it.
      */
    def elementClass: Class[?] = raw.getClass.getComponentType

    /** Returns the layout description string. Delegates to [[Layout.toString]].
      *
      * Renamed from `layout` (which now refers to the [[Layout]] value) to `layoutString`. The string format is
      * unchanged: "rows: …, cols: …, rowStride: …, colStride: …, offset: …, data length: …".
      */
    def layoutString: String = layout.toString
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
        layout = new Layout(rows, cols, rowStride, colStride, offset, raw.size, Layout.Strided)
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
      new Matrix(raw = raw, layout = new Layout(rows, cols, rowStride, colStride, offset, raw.size, Layout.Strided))
    end apply

    def apply(raw: Array[Float], rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Matrix[Float] =
      strideMatInstantiateCheck(raw, rows, cols, rowStride, colStride, offset)
      new Matrix(raw = raw, layout = new Layout(rows, cols, rowStride, colStride, offset, raw.size, Layout.Strided))
    end apply

    def apply(raw: Array[Int], rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Matrix[Int] =
      strideMatInstantiateCheck(raw, rows, cols, rowStride, colStride, offset)
      new Matrix(raw = raw, layout = new Layout(rows, cols, rowStride, colStride, offset, raw.size, Layout.Strided))
    end apply

    def apply(raw: Array[Long], rows: Row, cols: Col, rowStride: Int, colStride: Int, offset: Int): Matrix[Long] =
      strideMatInstantiateCheck(raw, rows, cols, rowStride, colStride, offset)
      new Matrix(raw = raw, layout = new Layout(rows, cols, rowStride, colStride, offset, raw.size, Layout.Strided))
    end apply

    def apply(
        raw: Array[Boolean],
        rows: Row,
        cols: Col,
        rowStride: Int,
        colStride: Int,
        offset: Int
    ): Matrix[Boolean] =
      strideMatInstantiateCheck(raw, rows, cols, rowStride, colStride, offset)
      new Matrix(raw = raw, layout = new Layout(rows, cols, rowStride, colStride, offset, raw.size, Layout.Strided))
    end apply

    /** Constructs a checked `Matrix` from an existing [[Layout]]. Throws `IllegalArgumentException` if
      * `raw.size != layout.dataLength` — that mismatch would mean the array is too small (or too large) for the layout
      * that was promised at construction time. Also delegates to [[strideMatInstantiateCheck]] to verify that every
      * element reachable via the layout's strides and offset falls within `raw`'s bounds.
      */
    def apply[A](raw: Array[A], layout: Layout): Matrix[A] =
      if raw.size != layout.dataLength then
        throw new IllegalArgumentException(
          s"raw array length ${raw.size} does not match layout.dataLength ${layout.dataLength}"
        )
      end if
      strideMatInstantiateCheck(raw, layout.rows, layout.cols, layout.rowStride, layout.colStride, layout.offset)
      new Matrix(raw, layout)
    end apply

    def apply[A](raw: Array[A], dim: RowCol): Matrix[A] =
      dimMatInstantiateCheck(raw, dim)

      new Matrix(
        raw = raw,
        layout = new Layout(dim._1, dim._2, 1, dim._1, 0, raw.size, Layout.Strided)
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
        layout = new Layout(rows, cols, 1, rows, 0, raw.size, Layout.Strided)
      )
    end apply

    def apply[A](dim: RowCol, raw: Array[A]): Matrix[A] =
      Matrix(raw, dim._1, dim._2)
    end apply

    /** Unchecked internal constructor — use only when the invariant `raw.size == layout.dataLength` has already been
      * proven at the call site (e.g., the array was just created with exactly that length, or it comes from an existing
      * checked `Matrix`). Mirrors `ndarray.mkNDArray`.
      */
    private[vecxt] def mkMatrix[A](raw: Array[A], layout: Layout): Matrix[A] = new Matrix(raw, layout)
  end Matrix

  extension [A](m: Matrix[A])

    // transparent inline def refinedRaw = m.raw

    def shape: RowCol = (m.rows, m.cols)

    // inline def rows: Row = m._2

    // inline def cols: Col = m._3

    // inline def numel: Int = m.raw.length

  end extension

end matrix
