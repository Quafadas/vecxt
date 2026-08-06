package vecxt

import munit.FunSuite

import all.*

/** Regression coverage for `update(idx: Matrix[Boolean], value)` and `updateInPlace` ignoring a view's offset and
  * strides, and for `update(idx, value)` additionally assuming `idx` shares `m`'s own physical layout. Both used to
  * index the backing array linearly (`m.raw(i)` for `i` in `0 until m.numel`), which is only valid for a matrix that is
  * dense, offset-free, and — for the boolean-mask overload — laid out identically to the mask.
  *
  * Every test here builds a `parent` matrix and either updates it directly or through a `submatrix` view sharing its
  * backing array, then asserts the *entire* parent grid — not just the cells the operation targets — so a write that
  * lands outside the view (the actual failure mode pre-fix) shows up as a mismatch at the wrong cell, not merely a
  * missing update at the right one.
  */
class ViewUpdateSuite extends FunSuite:

  test("boolean-mask update on an offset view only touches the view's own cells") {
    val parent = Matrix.fromRows(
      Array(1.0, 2.0, 3.0, 4.0),
      Array(5.0, 6.0, 7.0, 8.0),
      Array(9.0, 10.0, 11.0, 12.0),
      Array(13.0, 14.0, 15.0, 16.0)
    )
    val view = parent(Range.Inclusive(1, 2, 1), Range.Inclusive(1, 2, 1)) // 2x2 zero-copy view, offset != 0
    val mask = Matrix[Boolean](Array(true, false, false, true), 2, 2) // true on the view's own diagonal

    view(mask) = 0.0

    assertMatrixEquals(
      parent,
      Matrix.fromRows(
        Array(1.0, 2.0, 3.0, 4.0),
        Array(5.0, 0.0, 7.0, 8.0), // parent(1,1) zeroed; parent(1,2) untouched (mask false)
        Array(9.0, 10.0, 0.0, 12.0), // parent(2,1) untouched (mask false); parent(2,2) zeroed
        Array(13.0, 14.0, 15.0, 16.0)
      )
    )
  }

  test("boolean-mask update where mask and view have opposite physical layouts") {
    val parent = Matrix.fromRows(
      Array(1.0, 2.0, 3.0),
      Array(4.0, 5.0, 6.0),
      Array(7.0, 8.0, 9.0)
    )
    val view =
      parent(Range.Inclusive(1, 2, 1), Range.Inclusive(1, 2, 1)) // 2x2 view: [[5,6],[8,9]], col-major-inherited
    // Row-major mask, opposite of the view's inherited (col-major) layout. sameElementOrderAs must be false here,
    // so this also independently forces the general path even if the view had no offset at all.
    val mask = Matrix[Boolean](Array[Boolean](true, true, false, false), 2, 2, 2, 1, 0)

    view(mask) = -1.0

    assertMatrixEquals(
      parent,
      Matrix.fromRows(
        Array(1.0, 2.0, 3.0),
        Array(4.0, -1.0, -1.0), // parent(1,1), parent(1,2): mask row 0 is all true
        Array(7.0, 8.0, 9.0) // parent(2,1), parent(2,2): mask row 1 is all false, untouched
      )
    )
  }

  test("updateInPlace on an offset view only touches the view's own cells") {
    val parent = Matrix.fromRows(
      Array(1.0, 2.0, 3.0, 4.0),
      Array(5.0, 6.0, 7.0, 8.0),
      Array(9.0, 10.0, 11.0, 12.0),
      Array(13.0, 14.0, 15.0, 16.0)
    )
    val view = parent(Range.Inclusive(1, 2, 1), Range.Inclusive(1, 2, 1)) // 2x2 view: [[6,7],[10,11]]

    view.updateInPlace(::, Array(0), Array(100.0, 200.0)) // overwrite the view's own column 0

    assertMatrixEquals(
      parent,
      Matrix.fromRows(
        Array(1.0, 2.0, 3.0, 4.0),
        Array(5.0, 100.0, 7.0, 8.0), // parent(1,1) overwritten; parent(1,2) (view's column 1) untouched
        Array(9.0, 200.0, 11.0, 12.0), // parent(2,1) overwritten; parent(2,2) untouched
        Array(13.0, 14.0, 15.0, 16.0)
      )
    )
  }

  test("deepCopy round-trip: a boolean-mask update on a view matches the same update on a plain copy") {
    val parent = Matrix.fromRows(
      Array(1.0, 2.0, 3.0, 4.0),
      Array(5.0, 6.0, 7.0, 8.0),
      Array(9.0, 10.0, 11.0, 12.0),
      Array(13.0, 14.0, 15.0, 16.0)
    )
    val view = parent(Range.Inclusive(1, 2, 1), Range.Inclusive(1, 2, 1)) // 2x2 view: [[6,7],[10,11]], offset != 0
    val copy = view.deepCopy // fresh, dense, offset-0 matrix with the same logical contents
    val mask = Matrix[Boolean](Array(true, false, false, true), 2, 2)

    view(mask) = -5.0
    copy(mask) = -5.0

    for
      i <- 0 until view.rows
      j <- 0 until view.cols
    do assertEqualsDouble(view(i, j), copy(i, j), 0.0, s"at ($i, $j)")
    end for

    // The view's own update must not have leaked into the parent's untouched cells either.
    assertMatrixEquals(
      parent,
      Matrix.fromRows(
        Array(1.0, 2.0, 3.0, 4.0),
        Array(5.0, -5.0, 7.0, 8.0),
        Array(9.0, 10.0, -5.0, 12.0),
        Array(13.0, 14.0, 15.0, 16.0)
      )
    )
  }

end ViewUpdateSuite
