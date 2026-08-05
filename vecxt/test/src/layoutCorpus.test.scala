package vecxt

import munit.FunSuite

import matrix.*
import all.*

/** Cross-platform property test: `op(view) == op(copy)` for every elementwise `Matrix[Double]` operation, over a small,
  * deterministic, structured corpus of [[Layout]] "kinds" (not random values).
  *
  * Deliberately not scalacheck/random: the same fixed corpus is exercised on JVM, JS, and Native, which is what makes
  * this a genuine cross-platform consistency gate rather than "some tests passed everywhere" with a different sample
  * per platform. It is also small (a few dozen cases) and runs in milliseconds.
  *
  * The oracle (`model`) is a deliberately independent, "stupid" implementation living only in this test file: it reads
  * `raw` through nothing but `offset + i * rowStride + j * colStride` arithmetic, and never calls `foreach2D`,
  * `linearIndex`, or any other production code path. If the oracle shared code with the implementation under test (e.g.
  * by materialising a view via `foreach2D`), a row/col-major bug in `foreach2D` itself — like the one this test was
  * written to catch in `DoubleMatrix.maximum` — would silently cancel out on both sides.
  *
  * Comparison is logical, not raw: `assertLogicallyEqual` walks `(i, j)` through each matrix's own layout and reads
  * `raw` through the model arithmetic, rather than comparing the two backing arrays element-by-element. A transposed
  * result (rows/cols swapped, or a column-major destination filled in row-major order) has different `raw` arrays but
  * the same shape and *would* pass a raw-array comparison in the wrong direction, or fail to detect a swap between two
  * equal-sized dimensions — it's the logical `(i, j)` walk over each side's own dimensions that exposes it.
  *
  * Values are `Array.tabulate(i => (i + 1).toDouble)` — distinct per-element and non-square in every corpus entry, so a
  * transposition or an index swap changes an actual value at some `(i, j)` rather than silently aliasing two equal
  * inputs.
  */
class LayoutCorpusSuite extends FunSuite:

  private enum LayoutKind:
    case RowMajor, ColMajor, RowMajorPadded, ColMajorPadded, DoublyStrided
  end LayoutKind
  import LayoutKind.*

  /** Builds a `(backing array, Layout)` pair of the given kind, for testing purposes only — production code always goes
    * through the checked `Matrix` factories; this bypasses them deliberately to reach layout shapes (e.g. padding,
    * non-unit non-adjacent strides) that the factories wouldn't normally produce standalone.
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

  /** The independent oracle: reads `(i, j)` through nothing but layout arithmetic — no `foreach2D`, no `linearIndex`,
    * no shared code with the implementation under test.
    */
  private def model(m: Matrix[Double])(i: Int, j: Int): Double =
    m.raw(m.layout.offset + i * m.layout.rowStride + j * m.layout.colStride)

  /** Compares two matrices logically: same shape, and the same value at every `(i, j)` as read through each side's own
    * layout — not a raw-array comparison. Uses a small tolerance rather than exact equality: fast-path (BLAS/SIMD) and
    * `foreach2D` branches are not guaranteed to be bit-identical (e.g. true division vs. reciprocal-multiply can differ
    * by an ulp), and the property under test is numerical equivalence, not bit-for-bit reproduction of a specific
    * rounding strategy.
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
    do assertEqualsDouble(gotModel(i, j), wantModel(i, j), 1e-9, s"$clue at ($i, $j)")
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

  /** The set of raw-array indices reachable through `m`'s own `(row, col)` coordinates, i.e. "inside the view". Any raw
    * index not in this set is memory the view does not own.
    */
  private def reachableIndices(m: Matrix[Double]): Set[Int] =
    (for
      i <- 0 until m.rows
      j <- 0 until m.cols
    yield m.layout.linearIndex(i, j)).toSet
  end reachableIndices

  /** Asserts every raw-array slot NOT reachable via `m`'s own `(offset, rowStride, colStride)` is bit-for-bit unchanged
    * from `before`. This is a different assertion from `op(view) == op(copy)`: it catches an in-place op that corrupts
    * memory outside the view it was called on — the vecxt/pull/123 bug class (missing `else`, or a fast path that
    * mutates the whole backing array when the view doesn't own all of it) — which `op(view) == op(copy)` alone cannot
    * detect, since that property only inspects the view's own coordinates.
    */
  private def assertOutsideViewUntouched(m: Matrix[Double], before: Array[Double], clue: String)(implicit
      loc: munit.Location
  ): Unit =
    val reachable = reachableIndices(m)
    for idx <- before.indices if !reachable.contains(idx) do
      assertEqualsDouble(m.raw(idx), before(idx), 0.0, s"$clue: raw($idx) outside the view was mutated")
    end for
  end assertOutsideViewUntouched

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

  test("*(scalar) — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      assertLogicallyEqual(m.*(3.0), copy.*(3.0), s"*(3.0) on $m")
    end for
  }

  test("+:+ — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      val other = denseCopy(m).+(1.0) // same shape, distinct offset values
      val otherView = m.+(1.0)
      assertLogicallyEqual(m.+:+(otherView), copy.+:+(other), s"+:+ on $m")
    end for
  }

  test("-:- — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      val other = denseCopy(m).+(1.0)
      val otherView = m.+(1.0)
      assertLogicallyEqual(m.-:-(otherView), copy.-:-(other), s"-:- on $m")
    end for
  }

  test("/:/ — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      val other = denseCopy(m).+(1.0)
      val otherView = m.+(1.0)
      assertLogicallyEqual(m./:/(otherView), copy./:/(other), s"/:/ on $m")
    end for
  }

  test("*:* (boolean mask) — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      val mask = Matrix[Boolean](Array.tabulate(m.numel)(i => i % 2 == 0), m.rows, m.cols)
      assertLogicallyEqual(m.*:*(mask), copy.*:*(mask), s"*:* on $m")
    end for
  }

  test("exp — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      assertLogicallyEqual(m.exp, copy.exp, s"exp on $m")
    end for
  }

  test("log — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      assertLogicallyEqual(m.log, copy.log, s"log on $m")
    end for
  }

  test("sqrt — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      assertLogicallyEqual(m.sqrt, copy.sqrt, s"sqrt on $m")
    end for
  }

  test("sin — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      assertLogicallyEqual(m.sin, copy.sin, s"sin on $m")
    end for
  }

  test("cos — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      assertLogicallyEqual(m.cos, copy.cos, s"cos on $m")
    end for
  }

  /** The oracle for `Matrix[Boolean]` results — mirrors `model` above but for comparison-operator output. */
  private def modelBool(m: Matrix[Boolean])(i: Int, j: Int): Boolean =
    m.raw(m.layout.offset + i * m.layout.rowStride + j * m.layout.colStride)

  /** Mirrors `assertLogicallyEqual` but for `Matrix[Boolean]` results — used by the comparison operators
    * (`>=`/`>`/`<=`/`<`), which had the same `m.shape` mislabeling bug as exp/log/sqrt/sin/cos/tan/unary_-/**: their
    * fast path wrapped the result with `m.shape` (always column-major) instead of `m.layout`, silently transposing
    * the result for a dense row-major view.
    */
  private def assertLogicallyEqualBool(got: Matrix[Boolean], want: Matrix[Boolean], clue: String)(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(got.rows, want.rows, s"$clue: row count mismatch")
    assertEquals(got.cols, want.cols, s"$clue: col count mismatch")
    val gotModel = modelBool(got)
    val wantModel = modelBool(want)
    for
      i <- 0 until want.rows
      j <- 0 until want.cols
    do assertEquals(gotModel(i, j), wantModel(i, j), s"$clue at ($i, $j)")
    end for
  end assertLogicallyEqualBool

  test(">=(scalar) — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      assertLogicallyEqualBool(m.>=(3.0), copy.>=(3.0), s">= on $m")
    end for
  }

  test(">(scalar) — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      assertLogicallyEqualBool(m.>(3.0), copy.>(3.0), s"> on $m")
    end for
  }

  test("<=(scalar) — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      assertLogicallyEqualBool(m.<=(3.0), copy.<=(3.0), s"<= on $m")
    end for
  }

  test("<(scalar) — op(view) == op(copy) over the full layout-kind corpus") {
    for m <- corpus do
      val copy = denseCopy(m)
      assertLogicallyEqualBool(m.<(3.0), copy.<(3.0), s"< on $m")
    end for
  }

  // ---------------------------------------------------------------------------------------------------------------
  // In-place (`!`/`=`) operations. These carry a second property the tests above don't check: elements outside the
  // view — raw-array slots not reachable via (offset, rowStride, colStride) — must be untouched. That's exactly the
  // vecxt/pull/123 bug class (fixed there for sqrt!/sin!/cos!, with no gate to stop the same bug recurring in a
  // fourth method), so every in-place op below asserts both properties: the mutated view matches op(copy), and
  // everything outside the view is bit-for-bit unchanged.
  // ---------------------------------------------------------------------------------------------------------------

  test("*= (scalar) in-place — in-view matches op(copy), out-of-view untouched") {
    for m <- corpus do
      val copy = denseCopy(m)
      val before = m.raw.clone()
      copy.*=(3.0)
      m.*=(3.0)
      assertLogicallyEqual(m, copy, s"*= on $m")
      assertOutsideViewUntouched(m, before, s"*= on $m")
    end for
  }

  test("exp! in-place — in-view matches op(copy), out-of-view untouched") {
    for m <- corpus do
      val copy = denseCopy(m)
      val before = m.raw.clone()
      copy.`exp!`
      m.`exp!`
      assertLogicallyEqual(m, copy, s"exp! on $m")
      assertOutsideViewUntouched(m, before, s"exp! on $m")
    end for
  }

  test("log! in-place — in-view matches op(copy), out-of-view untouched") {
    for m <- corpus do
      val copy = denseCopy(m)
      val before = m.raw.clone()
      copy.`log!`
      m.`log!`
      assertLogicallyEqual(m, copy, s"log! on $m")
      assertOutsideViewUntouched(m, before, s"log! on $m")
    end for
  }

  test("sqrt! in-place — in-view matches op(copy), out-of-view untouched") {
    for m <- corpus do
      val copy = denseCopy(m)
      val before = m.raw.clone()
      copy.`sqrt!`
      m.`sqrt!`
      assertLogicallyEqual(m, copy, s"sqrt! on $m")
      assertOutsideViewUntouched(m, before, s"sqrt! on $m")
    end for
  }

  test("sin! in-place — in-view matches op(copy), out-of-view untouched") {
    for m <- corpus do
      val copy = denseCopy(m)
      val before = m.raw.clone()
      copy.`sin!`
      m.`sin!`
      assertLogicallyEqual(m, copy, s"sin! on $m")
      assertOutsideViewUntouched(m, before, s"sin! on $m")
    end for
  }

  test("cos! in-place — in-view matches op(copy), out-of-view untouched") {
    for m <- corpus do
      val copy = denseCopy(m)
      val before = m.raw.clone()
      copy.`cos!`
      m.`cos!`
      assertLogicallyEqual(m, copy, s"cos! on $m")
      assertOutsideViewUntouched(m, before, s"cos! on $m")
    end for
  }

  test("update(fct, value) in-place — in-view matches op(copy), out-of-view untouched") {
    for m <- corpus do
      val copy = denseCopy(m)
      val before = m.raw.clone()
      copy.update((x: Double) => x > 3.0, -1.0)
      m.update((x: Double) => x > 3.0, -1.0)
      assertLogicallyEqual(m, copy, s"update(fct, value) on $m")
      assertOutsideViewUntouched(m, before, s"update(fct, value) on $m")
    end for
  }

end LayoutCorpusSuite
