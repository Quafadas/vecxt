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
    case RowMajor, ColMajor, RowMajorPadded, ColMajorPadded, DoublyStrided, ColMajorLeadingCols, RowMajorLeadingRows
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
      case ColMajorLeadingCols =>
        // Dense col-major, offset 0 (unit rowStride, colStride == rows) — same as ColMajor — but the backing array
        // extends past rows*cols. This is what `submatrix` produces for the leading columns of a wider parent: a
        // genuine zero-copy view that is dense and offset-0 but doesn't own its entire backing array. `offset` is
        // intentionally ignored — a nonzero offset would already fail `isDenseColMajor` for an unrelated reason,
        // which isn't the gap this kind exists to cover.
        val len = rows * cols + 5
        (Array.tabulate(len)(i => (i + 1).toDouble), Layout(rows, cols, 1, rows, 0, len))
      case RowMajorLeadingRows =>
        // Mirror case: dense row-major, offset 0, backing array extends past rows*cols — e.g. the leading rows of a
        // taller parent matrix.
        val len = rows * cols + 5
        (Array.tabulate(len)(i => (i + 1).toDouble), Layout(rows, cols, cols, 1, 0, len))
  end mkLayout

  private val dims = List((1, 1), (1, 4), (4, 1), (2, 3), (3, 2), (3, 3))
  private val kinds =
    List(RowMajor, ColMajor, RowMajorPadded, ColMajorPadded, DoublyStrided, ColMajorLeadingCols, RowMajorLeadingRows)
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
      // The mask shares m's own layout — not always column-major, as a freshly built one (e.g. via the (rows, cols)
      // factory) would be — so `sameDenseElementWiseMemoryLayoutCheck` reaches the fast path for BOTH dense
      // orientations, not just column-major. This matters because on JS/Native that fast path used to fill its
      // result array in m's own storage order, then wrap it with a hardcoded column-major layout — silently
      // transposing the result whenever m was dense row-major.
      //
      // "want" is computed directly from the independent model oracle rather than by calling `.*:*` on
      // `denseCopy(m)`: when m is dense row-major, `denseCopy` builds a layout that is structurally identical to
      // m's own, so a row-major-shaped mask would make m and its copy take the exact same fast path and cancel out
      // an identical bug on both sides — precisely the failure mode this file's top comment warns about.
      val mModel = model(m)
      val mask = Matrix[Boolean](Array.tabulate(m.layout.dataLength)(i => i % 2 == 0), m.layout)
      val maskModel = modelBool(mask)
      val wantArr = Array.ofDim[Double](m.rows * m.cols)
      for
        i <- 0 until m.rows
        j <- 0 until m.cols
      do wantArr(i + j * m.rows) = if maskModel(i, j) then mModel(i, j) else 0.0
      end for
      val want = Matrix[Double](wantArr, m.rows, m.cols)
      assertLogicallyEqual(m.*:*(mask), want, s"*:* on $m")
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
    * (`>=`/`>`/`<=`/`<`), which had the same `m.shape` mislabeling bug as exp/log/sqrt/sin/cos/tan/unary_- and the
    * power operator: their fast path wrapped the result with `m.shape` (always column-major) instead of `m.layout`,
    * silently transposing the result for a dense row-major view.
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

  test("*:*= (boolean mask) in-place — in-view matches op(copy), out-of-view untouched") {
    // *:*= was the one mutating op missing from this suite: it looped to `m.raw.length` instead of `m.numel`,
    // relying entirely on `sameElementOrderAs` to keep the fast path from firing on a view that doesn't own its
    // whole backing array (e.g. ColMajorLeadingCols / RowMajorLeadingRows above). Computing `want` from the
    // independent model oracle before mutating, rather than from a second `*:*=` call, keeps this test from
    // sharing code with the implementation under test.
    for m <- corpus do
      val before = m.raw.clone()
      val mModel = model(m)
      val mask = Matrix[Boolean](Array.tabulate(m.layout.dataLength)(i => i % 2 == 0), m.layout)
      val maskModel = modelBool(mask)
      val wantArr = Array.ofDim[Double](m.rows * m.cols)
      for
        i <- 0 until m.rows
        j <- 0 until m.cols
      do wantArr(i + j * m.rows) = if maskModel(i, j) then mModel(i, j) else 0.0
      end for
      val want = Matrix[Double](wantArr, m.rows, m.cols)

      m.*:*=(mask)

      assertLogicallyEqual(m, want, s"*:*= on $m")
      assertOutsideViewUntouched(m, before, s"*:*= on $m")
    end for
  }

  test("*:*= via submatrix — leading-columns view must not corrupt the parent's trailing columns") {
    // Direct regression test for the reported exploit, through the real production `submatrix` path rather than
    // the synthetic `mkLayout` corpus: `m.submatrix(0 to R-1, 0 to k-1)` on a column-major parent is a genuine
    // zero-copy view sharing the parent's backing array and strides, with `dataLength` (the parent's full size)
    // greater than `numel` (the view's own element count) — exactly the shape `sameElementOrderAs` used to accept.
    val rows = 4
    val parentCols = 6
    val subCols = 3
    val parent = Matrix[Double](Array.tabulate(rows * parentCols)(i => (i + 1).toDouble), rows, parentCols)
    val trailingStart = rows * subCols
    val trailingBefore = Array.tabulate(rows * (parentCols - subCols))(k => parent.raw(trailingStart + k))

    val sub = parent.submatrix(0 to rows - 1, 0 to subCols - 1)
    val mask = Matrix[Boolean](Array.tabulate(rows * subCols)(i => i % 2 == 0), rows, subCols)
    sub.*:*=(mask)

    for k <- 0 until rows * (parentCols - subCols) do
      assertEqualsDouble(
        parent.raw(trailingStart + k),
        trailingBefore(k),
        0.0,
        s"*:*= via submatrix corrupted parent raw(${trailingStart + k}), which the view does not own"
      )
    end for
  }

  // ---------------------------------------------------------------------------------------------------------------
  // `hadamard`'s mismatched-layout branches used to guard on `isDenseColMajor`/`isDenseRowMajor` alone, then
  // multiply that operand's `.raw` directly against a freshly `numel`-sized `deepCopy` of the other side via an
  // array op that requires the two arrays to be exactly the same length. `isDenseColMajor`/`isDenseRowMajor` do not
  // imply `raw.length == numel` — a `submatrix` view of the leading columns of a wider parent is dense by that
  // narrower definition but keeps the parent's full backing array — so multiplying it directly against a
  // `numel`-sized array threw `VectorDimensionMismatch` for an entirely valid, correctly-shaped `hadamard` call.
  // ---------------------------------------------------------------------------------------------------------------

  test("hadamard: a dense-but-padded leading-columns view as the first operand doesn't throw") {
    val parent = Matrix[Double](Array.tabulate(20)(_.toDouble + 1), 4, 5) // 4x5 dense col-major
    val paddedView = parent.submatrix(0 to 3, 0 to 2) // leading 3 columns: isDenseColMajor, but raw.length (20) > numel (12)
    val plainDense = Matrix.fromRows(
      Array(1.0, 2.0, 3.0),
      Array(4.0, 5.0, 6.0),
      Array(7.0, 8.0, 9.0),
      Array(10.0, 11.0, 12.0)
    )

    val result = paddedView.hadamard(plainDense)

    assertMatrixEquals(
      result,
      Matrix.fromRows(
        Array(1.0, 10.0, 27.0),
        Array(8.0, 30.0, 60.0),
        Array(21.0, 56.0, 99.0),
        Array(40.0, 88.0, 144.0)
      )
    )
  }

  test("hadamard: a dense-but-padded leading-columns view as the second operand doesn't throw") {
    val parent = Matrix[Double](Array.tabulate(20)(_.toDouble + 1), 5, 4) // 5x4 dense col-major
    // An offset view: neither isDenseColMajor nor isDenseRowMajor (offset != 0), so entry into the branch below is
    // driven entirely by paddedView2 (the second operand), not by m itself.
    val m = parent.submatrix(0 to 3, 1 to 3)

    val parent2 = Matrix[Double](Array.tabulate(20)(_.toDouble + 1), 4, 5) // 4x5 dense col-major
    val paddedView2 = parent2.submatrix(0 to 3, 0 to 2) // leading 3 columns, same shape as m: 4x3

    val result = m.hadamard(paddedView2)

    assertMatrixEquals(
      result,
      Matrix.fromRows(
        Array(6.0, 55.0, 144.0),
        Array(14.0, 72.0, 170.0),
        Array(24.0, 91.0, 198.0),
        Array(36.0, 112.0, 228.0)
      )
    )
  }

end LayoutCorpusSuite
