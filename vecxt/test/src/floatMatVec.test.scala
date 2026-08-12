package vecxt

import all.*

/** `Matrix[Float] * Array[Float]` and its in-place kernel `*=`.
  *
  * Shared rather than JVM-only because the operation now exists on every platform: the JVM routes it through BLAS
  * `sgemv`, picking TRANS and lda from the strides, while JS and Native run an elementwise loop. Asserting the same
  * answers here is what holds those implementations to the same behaviour — previously the operation existed only on
  * the JVM, and only for dense column-major operands.
  *
  * Every fixture is the same logical 2x3 [[1,2,3],[4,5,6]] in a different layout, so all of them must agree.
  */
class FloatMatVecSuite extends munit.FunSuite:

  private def assertFloatArrayEquals(actual: Array[Float], expected: Array[Float])(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(actual.length, expected.length, "length mismatch")
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i).toDouble, expected(i).toDouble, 1e-4, clue = s"at index $i")
      i += 1
    end while
  end assertFloatArrayEquals

  private def colMajor = Matrix[Float](Array[Float](1.0f, 4.0f, 2.0f, 5.0f, 3.0f, 6.0f), 2, 3, 1, 2, 0)
  private def rowMajor = Matrix[Float](Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), 2, 3, 3, 1, 0)
  // rowStride 1, colStride 3 over a length-9 array: column j is raw(3j), raw(3j+1); raw(3j+2) is padding.
  private def paddedCol =
    Matrix[Float](Array[Float](1.0f, 4.0f, 99.0f, 2.0f, 5.0f, 99.0f, 3.0f, 6.0f, 99.0f), 2, 3, 1, 3, 0)
  // colStride 1, rowStride 4 over a length-8 array: row i is raw(4i)..raw(4i+2); raw(4i+3) is padding.
  private def paddedRow =
    Matrix[Float](Array[Float](1.0f, 2.0f, 3.0f, 99.0f, 4.0f, 5.0f, 6.0f, 99.0f), 2, 3, 4, 1, 0)
  // A 2x3 window at offset 5 of a 4x4 column-major parent; the 99.0s around it are the parent's own elements, which
  // a mis-derived index would pick up instead of the view's.
  private def offsetView =
    Matrix[Float](
      Array[Float](99.0f, 99.0f, 99.0f, 99.0f, 99.0f, 1.0f, 4.0f, 99.0f, 99.0f, 2.0f, 5.0f, 99.0f, 99.0f, 3.0f, 6.0f,
        99.0f),
      4,
      4
    ).submatrix(1 to 2, 1 to 3)

  private def fixtures = List(colMajor, rowMajor, paddedCol, paddedRow, offsetView)

  test("the Float fixtures are all logically [[1,2,3],[4,5,6]]") {
    for m <- fixtures do
      assertEquals(m.shape, (2, 3), s"shape, layout ${m.layoutString}")
      for
        row <- 0 until 2
        col <- 0 until 3
      do
        assertEqualsDouble(m(row, col).toDouble, (row * 3 + col + 1).toDouble, 1e-9, s"at ($row, $col)")
      end for
    end for
  }

  test("Float matrix * vector agrees across every layout") {
    val x = Array[Float](1.0f, 2.0f, 3.0f)
    // [[1,2,3],[4,5,6]] * [1,2,3] = [1+4+9, 4+10+18]
    for m <- fixtures do
      assertFloatArrayEquals(m * x, Array[Float](14.0f, 32.0f))
    end for
  }

  test("Float matrix * vector honours alpha across every layout") {
    val x = Array[Float](1.0f, 1.0f, 1.0f)
    for m <- fixtures do
      assertFloatArrayEquals(m.*(x, 2.0f), Array[Float](12.0f, 30.0f))
    end for
  }

  test("Float matrix *= vector accumulates per beta across every layout") {
    val x = Array[Float](1.0f, 2.0f, 3.0f) // m @@ x is [14, 32]
    for m <- fixtures do
      val y = Array[Float](100.0f, 200.0f)
      m.*=(x, y, 1.0f, 1.0f)
      assertFloatArrayEquals(y, Array[Float](114.0f, 232.0f))

      val y2 = Array[Float](10.0f, 20.0f)
      m.*=(x, y2, 2.0f, 3.0f) // 2*[14,32] + 3*[10,20]
      assertFloatArrayEquals(y2, Array[Float](58.0f, 124.0f))
    end for
  }

  test("Float matrix *= vector with beta = 0 overwrites without reading the destination") {
    // Matches the Double contract: beta == 0 writes y without reading it, so a NaN destination must not propagate.
    val x = Array[Float](1.0f, 2.0f, 3.0f)
    for m <- fixtures do
      val y = Array[Float](Float.NaN, Float.NaN)
      m.*=(x, y, 1.0f, 0.0f)
      assertFloatArrayEquals(y, Array[Float](14.0f, 32.0f))
      assertFloatArrayEquals(y, m * x)
    end for
  }

  test("Float matrix * vector handles a broadcast column") {
    // colStride 0 repeats one column across the matrix; no leading dimension expresses that, so the guards reject it
    // and the elementwise branch runs. Rows are [2,2,2] and [3,3,3].
    val m = Matrix[Float](Array[Float](2.0f, 3.0f), 2, 3, 1, 0, 0)
    assertFloatArrayEquals(m * Array[Float](1.0f, 2.0f, 3.0f), Array[Float](12.0f, 18.0f))
  }

  test("Float matrix * vector rejects mismatched lengths") {
    intercept[IllegalArgumentException](rowMajor * Array[Float](1.0f, 2.0f))
    // alpha/beta spelled out: unlike the Double twin, the Float `*=` carries no defaults, because `all` exports both
    // into one scope and only one overload of a name may have them.
    intercept[IllegalArgumentException](
      rowMajor.*=(Array[Float](1.0f, 2.0f, 3.0f), Array[Float](0.0f, 0.0f, 0.0f), 1.0f, 0.0f)
    )
  }

end FloatMatVecSuite
