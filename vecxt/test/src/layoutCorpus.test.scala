package vecxt

import munit.FunSuite

import matrix.*
import all.*

/** Cross-platform property test: `op(view) == op(copy)` for every elementwise `Matrix[Double]` operation, over a
  * small, deterministic, structured corpus of [[Layout]] "kinds" (not random values).
  *
  * Deliberately not scalacheck/random: the same fixed corpus is exercised on JVM, JS, and Native, which is what makes
  * this a genuine cross-platform consistency gate rather than "some tests passed everywhere" with a different sample
  * per platform. It is also small (a few dozen cases) and runs in milliseconds.
  *
  * The oracle (`model`) is a deliberately independent, "stupid" implementation living only in this test file: it reads
  * `raw` through nothing but `offset + i * rowStride + j * colStride` arithmetic, and never calls `foreach2D`,
  * `linearIndex`, or any other production code path. If the oracle shared code with the implementation under test
  * (e.g. by materialising a view via `foreach2D`), a row/col-major bug in `foreach2D` itself — like the one this test
  * was written to catch in `DoubleMatrix.maximum` — would silently cancel out on both sides.
  *
  * Comparison is logical, not raw: `assertLogicallyEqual` walks `(i, j)` through each matrix's own layout and reads
  * `raw` through the model arithmetic, rather than comparing the two backing arrays element-by-element. A transposed
  * result (rows/cols swapped, or a column-major destination filled in row-major order) has different `raw` arrays but
  * the same shape and *would* pass a raw-array comparison in the wrong direction, or fail to detect a swap between two
  * equal-sized dimensions — it's the logical `(i, j)` walk over each side's own dimensions that exposes it.
  *
  * Values are `Array.tabulate(i => (i + 1).toDouble)` — distinct per-element and non-square in every corpus entry, so
  * a transposition or an index swap changes an actual value at some `(i, j)` rather than silently aliasing two equal
  * inputs.
  */
class LayoutCorpusSuite extends FunSuite:

  private enum LayoutKind:
    case RowMajor, ColMajor, RowMajorPadded, ColMajorPadded, DoublyStrided
  end LayoutKind
  import LayoutKind.*

  /** Builds a `(backing array, Layout)` pair of the given kind, for testing purposes only — production code always
    * goes through the checked `Matrix` factories; this bypasses them deliberately to reach layout shapes (e.g.
    * padding, non-unit non-adjacent strides) that the factories wouldn't normally produce standalone.
    */
  private def mkLayout(rows: Int, cols: Int, kind: LayoutKind, offset: Int): (Array[Double], Layout) =
    kind match
      case RowMajor =>
        val len = offset + rows * cols
        (Array.tabulate(len)(i => (i + 1).toDouble), Layout(rows, cols, cols, 1, offset, len))
      case ColMajor =>
        val len = offset + rows * cols
        (Array.tabulate(len)(i => (i + 1).toDouble), Layout(rows, cols, 1, rows, offset, len))
      case RowMajorPadded =>
        // one extra column of padding per row (colStride still 1, rowStride wider than cols)
        val rowStride = cols + 2
        val len = offset + rows * rowStride
        (Array.tabulate(len)(i => (i + 1).toDouble), Layout(rows, cols, rowStride, 1, offset, len))
      case ColMajorPadded =>
        // one extra row of padding per column (rowStride still 1, colStride wider than rows)
        val colStride = rows + 2
        val len = offset + cols * colStride
        (Array.tabulate(len)(i => (i + 1).toDouble), Layout(rows, cols, 1, colStride, offset, len))
      case DoublyStrided =>
        // neither stride is 1 — no unit-stride axis at all
        val rowStride = 2
        val colStride = rows * rowStride + 3
        val len = offset + (rows - 1) * rowStride + (cols - 1) * colStride + 1
        (Array.tabulate(len)(i => (i + 1).toDouble), Layout(rows, cols, rowStride, colStride, offset, len))
  end mkLayout

  private val dims = List((1, 1), (1, 4), (4, 1), (2, 3), (3, 2), (3, 3))
  private val kinds = List(RowMajor, ColMajor, RowMajorPadded, ColMajorPadded, DoublyStrided)
  private val offsets = List(0, 1, 7)

  private def corpus: Seq[Matrix[Double]] =
    for
      (r, c) <- dims
      kind <- kinds
      offset <- offsets
    yield
      val (raw, layout) = mkLayout(r, c, kind, offset)
      Matrix[Double](raw, layout)

  /** The independent oracle: reads `(i, j)` through nothing but layout arithmetic — no `foreach2D`, no
    * `linearIndex`, no shared code with the implementation under test.
    */
  private def model(m: Matrix[Double])(i: Int, j: Int): Double =
    m.raw(m.layout.offset + i * m.layout.rowStride + j * m.layout.colStride)

  /** Compares two matrices logically: same shape, and the same value at every `(i, j)` as read through each side's
    * own layout — not a raw-array comparison.
    */
  private def assertLogicallyEqual(got: Matrix[Double], want: Matrix[Double], clue: String)(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(got.rows, want.rows, s"$clue: row count mismatch")
    assertEquals(got.cols, want.cols, s"$clue: col count mismatch")
    val gotModel = model(got)
    val wantModel = model(want)
    for
      i <- 0 until want.rows
      j <- 0 until want.cols
    do assertEquals(gotModel(i, j), wantModel(i, j), s"$clue at ($i, $j)")
    end for
  end assertLogicallyEqual

  /** A freshly allocated, dense row-major copy of `m`'s logical contents — built purely from the oracle, so it shares
    * no code with any `Matrix` elementwise operation.
    */
  private def denseCopy(m: Matrix[Double]): Matrix[Double] =
    val mm = model(m)
    val out = Array.ofDim[Double](m.rows * m.cols)
    var i = 0
    while i < m.rows do
      var j = 0
      while j < m.cols do
        out(i * m.cols + j) = mm(i, j)
        j += 1
      end while
      i += 1
    end while
    Matrix[Double](out, Layout(m.rows, m.cols, m.cols, 1, 0, m.rows * m.cols))
  end denseCopy

  test("maximum(other) — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      val other = denseCopy(m).+(1.0) // same shape, distinct offset values
      val otherView = m.+(1.0)

      val gotFromView = m.maximum(otherView)
      val wantFromCopy = copy.maximum(other)
      assertLogicallyEqual(gotFromView, wantFromCopy, s"maximum on $m")
    end for
  }

  test("+(scalar) — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      assertLogicallyEqual(m.+(3.0), copy.+(3.0), s"+(3.0) on $m")
    end for
  }

  test("-(scalar) — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      assertLogicallyEqual(m.-(3.0), copy.-(3.0), s"-(3.0) on $m")
    end for
  }

  test("/(scalar) — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      assertLogicallyEqual(m./(3.0), copy./(3.0), s"/(3.0) on $m")
    end for
  }

end LayoutCorpusSuite
