package vecxt

import munit.FunSuite

import all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

class FloatMatrixJvmSuite extends FunSuite:

  private val tolerance = 1e-4

  private def assertFloatVecEquals(actual: Array[Float], expected: Array[Float])(implicit loc: munit.Location): Unit =
    assertEquals(actual.length, expected.length, "vector length mismatch")
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i).toDouble, expected(i).toDouble, tolerance, clue = s"at index $i")
      i += 1
    end while
  end assertFloatVecEquals

  private def assertFloatMatrixEquals(actual: Matrix[Float], expected: Matrix[Float])(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(actual.shape, expected.shape, "matrix shape mismatch")
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col).toDouble, expected(row, col).toDouble, tolerance, clue = s"at ($row, $col)")
        col += 1
      end while
      row += 1
    end while
  end assertFloatMatrixEquals

  private def assertDoubleMatrixEquals(actual: Matrix[Double], expected: Matrix[Double])(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(actual.shape, expected.shape, "matrix shape mismatch")
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance, clue = s"at ($row, $col)")
        col += 1
      end while
      row += 1
    end while
  end assertDoubleMatrixEquals

  private def assertBooleanMatrixEquals(actual: Matrix[Boolean], expected: Matrix[Boolean])(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(actual.shape, expected.shape, "matrix shape mismatch")
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEquals(actual(row, col), expected(row, col), clue = s"at ($row, $col)")
        col += 1
      end while
      row += 1
    end while
  end assertBooleanMatrixEquals

  test("*:* on dense Float matrix yields expected Double matrix"):
    val mat = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f, 3.0f),
      Array[Float](4.0f, 5.0f, 6.0f)
    )

    val mask = Matrix.fromRows[Boolean](
      Array[Boolean](true, false, true),
      Array[Boolean](false, true, false)
    )

    val result = mat *:* mask

    assertDoubleMatrixEquals(
      result,
      Matrix.fromRows[Double](
        Array[Double](1.0, 0.0, 3.0),
        Array[Double](0.0, 5.0, 0.0)
      )
    )

  test("*:* on offset Float view uses general layout path"):
    val base = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f, 3.0f, 4.0f),
      Array[Float](5.0f, 6.0f, 7.0f, 8.0f),
      Array[Float](9.0f, 10.0f, 11.0f, 12.0f)
    )
    val sub = base(Range.Inclusive(0, 2, 1), Range.Inclusive(1, 2, 1))

    val mask = Matrix.fromRows[Boolean](
      Array[Boolean](false, true),
      Array[Boolean](true, false),
      Array[Boolean](false, true)
    )

    val result = sub *:* mask

    assertDoubleMatrixEquals(
      result,
      Matrix.fromRows[Double](
        Array[Double](0.0, 3.0),
        Array[Double](6.0, 0.0),
        Array[Double](0.0, 11.0)
      )
    )

  test("*:*= on dense Float matrix mutates in place"):
    val mat = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f, 3.0f),
      Array[Float](4.0f, 5.0f, 6.0f)
    )

    val mask = Matrix.fromRows[Boolean](
      Array[Boolean](true, false, true),
      Array[Boolean](false, true, false)
    )

    mat *:*= mask

    assertFloatMatrixEquals(
      mat,
      Matrix.fromRows[Float](
        Array[Float](1.0f, 0.0f, 3.0f),
        Array[Float](0.0f, 5.0f, 0.0f)
      )
    )

  test("comparison operators on offset Float view return expected masks"):
    val base = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f, 3.0f, 4.0f),
      Array[Float](5.0f, 6.0f, 7.0f, 8.0f),
      Array[Float](9.0f, 10.0f, 11.0f, 12.0f)
    )
    val sub = base(Range.Inclusive(0, 2, 1), Range.Inclusive(1, 2, 1))

    assertBooleanMatrixEquals(
      sub >= 7.0f,
      Matrix.fromRows[Boolean](
        Array[Boolean](false, false),
        Array[Boolean](false, true),
        Array[Boolean](true, true)
      )
    )

    assertBooleanMatrixEquals(
      sub > 7.0f,
      Matrix.fromRows[Boolean](
        Array[Boolean](false, false),
        Array[Boolean](false, false),
        Array[Boolean](true, true)
      )
    )

    assertBooleanMatrixEquals(
      sub <= 7.0f,
      Matrix.fromRows[Boolean](
        Array[Boolean](true, true),
        Array[Boolean](true, true),
        Array[Boolean](false, false)
      )
    )

    assertBooleanMatrixEquals(
      sub < 7.0f,
      Matrix.fromRows[Boolean](
        Array[Boolean](true, true),
        Array[Boolean](true, false),
        Array[Boolean](false, false)
      )
    )

  test("+= scalar updates dense row-major Float matrix"):
    val rowMajor = Matrix[Float](
      Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f),
      2,
      3,
      3,
      1,
      0
    )

    rowMajor += 0.5f

    assertFloatMatrixEquals(
      rowMajor,
      Matrix.fromRows[Float](
        Array[Float](1.5f, 2.5f, 3.5f),
        Array[Float](4.5f, 5.5f, 6.5f)
      )
    )

  test("+= scalar updates Float matrix with row-stride ordered gather path"):
    val raw = Array[Float](1.0f, 99.0f, 2.0f, 99.0f, 3.0f, 99.0f, 4.0f, 99.0f, 5.0f, 99.0f, 6.0f, 99.0f)
    val mat = Matrix[Float](raw, 3, 2, 2, 6, 0)

    mat += 1.25f

    assertFloatMatrixEquals(
      mat,
      Matrix.fromRows[Float](
        Array[Float](2.25f, 5.25f),
        Array[Float](3.25f, 6.25f),
        Array[Float](4.25f, 7.25f)
      )
    )
    assertFloatVecEquals(raw.filter(_ == 99.0f), Array.fill[Float](6)(99.0f))

  test("+= scalar updates Float matrix with col-stride ordered gather path"):
    val raw = Array[Float](1.0f, 99.0f, 2.0f, 99.0f, 3.0f, 99.0f, 4.0f, 99.0f, 5.0f, 99.0f, 6.0f, 99.0f)
    val mat = Matrix[Float](raw, 2, 3, 6, 2, 0)

    mat += 2.0f

    assertFloatMatrixEquals(
      mat,
      Matrix.fromRows[Float](
        Array[Float](3.0f, 4.0f, 5.0f),
        Array[Float](6.0f, 7.0f, 8.0f)
      )
    )
    assertFloatVecEquals(raw.filter(_ == 99.0f), Array.fill[Float](6)(99.0f))

  test("-= scalar updates dense row-major Float matrix"):
    val rowMajor = Matrix[Float](
      Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f),
      2,
      3,
      3,
      1,
      0
    )

    rowMajor -= 0.5f

    assertFloatMatrixEquals(
      rowMajor,
      Matrix.fromRows[Float](
        Array[Float](0.5f, 1.5f, 2.5f),
        Array[Float](3.5f, 4.5f, 5.5f)
      )
    )

  test("-= scalar updates Float matrix with row-stride ordered gather path"):
    val raw = Array[Float](1.0f, 99.0f, 2.0f, 99.0f, 3.0f, 99.0f, 4.0f, 99.0f, 5.0f, 99.0f, 6.0f, 99.0f)
    val mat = Matrix[Float](raw, 3, 2, 2, 6, 0)

    mat -= 1.25f

    assertFloatMatrixEquals(
      mat,
      Matrix.fromRows[Float](
        Array[Float](-0.25f, 2.75f),
        Array[Float](0.75f, 3.75f),
        Array[Float](1.75f, 4.75f)
      )
    )
    assertFloatVecEquals(raw.filter(_ == 99.0f), Array.fill[Float](6)(99.0f))

  test("-= scalar updates Float matrix with col-stride ordered gather path"):
    val raw = Array[Float](1.0f, 99.0f, 2.0f, 99.0f, 3.0f, 99.0f, 4.0f, 99.0f, 5.0f, 99.0f, 6.0f, 99.0f)
    val mat = Matrix[Float](raw, 2, 3, 6, 2, 0)

    mat -= 2.0f

    assertFloatMatrixEquals(
      mat,
      Matrix.fromRows[Float](
        Array[Float](-1.0f, 0.0f, 1.0f),
        Array[Float](2.0f, 3.0f, 4.0f)
      )
    )
    assertFloatVecEquals(raw.filter(_ == 99.0f), Array.fill[Float](6)(99.0f))

  test("+= array broadcasts across dense and general-stride Float matrices"):
    val dense = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f, 3.0f),
      Array[Float](4.0f, 5.0f, 6.0f)
    )
    dense += Array[Float](10.0f, 20.0f, 30.0f)

    assertFloatMatrixEquals(
      dense,
      Matrix.fromRows[Float](
        Array[Float](11.0f, 22.0f, 33.0f),
        Array[Float](14.0f, 25.0f, 36.0f)
      )
    )

    val rowMajor = Matrix[Float](
      Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f),
      2,
      3,
      3,
      1,
      0
    )
    rowMajor += Array[Float](10.0f, 20.0f, 30.0f)

    assertFloatMatrixEquals(
      rowMajor,
      Matrix.fromRows[Float](
        Array[Float](11.0f, 22.0f, 33.0f),
        Array[Float](14.0f, 25.0f, 36.0f)
      )
    )

    val sparseRaw = Array[Float](1.0f, 91.0f, 2.0f, 92.0f, 93.0f, 3.0f, 94.0f, 4.0f)
    val strided = Matrix[Float](sparseRaw, 2, 2, 2, 5, 0)
    strided += Array[Float](10.0f, 20.0f)

    assertFloatMatrixEquals(
      strided,
      Matrix.fromRows[Float](
        Array[Float](11.0f, 23.0f),
        Array[Float](12.0f, 24.0f)
      )
    )
    assertFloatVecEquals(sparseRaw.filter(_ >= 91.0f), Array[Float](91.0f, 92.0f, 93.0f, 94.0f))

  test("-= array broadcasts across dense and general-stride Float matrices"):
    val dense = Matrix.fromRows[Float](
      Array[Float](11.0f, 22.0f, 33.0f),
      Array[Float](14.0f, 25.0f, 36.0f)
    )
    dense -= Array[Float](10.0f, 20.0f, 30.0f)

    assertFloatMatrixEquals(
      dense,
      Matrix.fromRows[Float](
        Array[Float](1.0f, 2.0f, 3.0f),
        Array[Float](4.0f, 5.0f, 6.0f)
      )
    )

    val rowMajor = Matrix[Float](
      Array[Float](11.0f, 22.0f, 33.0f, 14.0f, 25.0f, 36.0f),
      2,
      3,
      3,
      1,
      0
    )
    rowMajor -= Array[Float](10.0f, 20.0f, 30.0f)

    assertFloatMatrixEquals(
      rowMajor,
      Matrix.fromRows[Float](
        Array[Float](1.0f, 2.0f, 3.0f),
        Array[Float](4.0f, 5.0f, 6.0f)
      )
    )

    val sparseRaw = Array[Float](11.0f, 91.0f, 12.0f, 92.0f, 93.0f, 23.0f, 94.0f, 24.0f)
    val strided = Matrix[Float](sparseRaw, 2, 2, 2, 5, 0)
    strided -= Array[Float](10.0f, 20.0f)

    assertFloatMatrixEquals(
      strided,
      Matrix.fromRows[Float](
        Array[Float](1.0f, 3.0f),
        Array[Float](2.0f, 4.0f)
      )
    )
    assertFloatVecEquals(sparseRaw.filter(_ >= 91.0f), Array[Float](91.0f, 92.0f, 93.0f, 94.0f))

  test("*= scalar mutates dense Float matrix in place"):
    val mat = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f, 3.0f),
      Array[Float](4.0f, 5.0f, 6.0f)
    )

    mat *= 2.5f

    assertFloatMatrixEquals(
      mat,
      Matrix.fromRows[Float](
        Array[Float](2.5f, 5.0f, 7.5f),
        Array[Float](10.0f, 12.5f, 15.0f)
      )
    )

  test("* scalar returns new Float matrix and leaves source unchanged"):
    val mat = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f, 3.0f),
      Array[Float](4.0f, 5.0f, 6.0f)
    )

    val result = mat * 2.0f

    assertFloatMatrixEquals(
      result,
      Matrix.fromRows[Float](
        Array[Float](2.0f, 4.0f, 6.0f),
        Array[Float](8.0f, 10.0f, 12.0f)
      )
    )
    assertFloatMatrixEquals(
      mat,
      Matrix.fromRows[Float](
        Array[Float](1.0f, 2.0f, 3.0f),
        Array[Float](4.0f, 5.0f, 6.0f)
      )
    )

  test("* scalar supports Float submatrix views via deep copy"):
    val base = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f, 3.0f, 4.0f),
      Array[Float](5.0f, 6.0f, 7.0f, 8.0f),
      Array[Float](9.0f, 10.0f, 11.0f, 12.0f)
    )
    val sub = base(Range.Inclusive(0, 2, 1), Range.Inclusive(1, 2, 1))

    val result = sub * 3.0f

    assertFloatMatrixEquals(
      result,
      Matrix.fromRows[Float](
        Array[Float](6.0f, 9.0f),
        Array[Float](18.0f, 21.0f),
        Array[Float](30.0f, 33.0f)
      )
    )
    assertFloatMatrixEquals(
      sub,
      Matrix.fromRows[Float](
        Array[Float](2.0f, 3.0f),
        Array[Float](6.0f, 7.0f),
        Array[Float](10.0f, 11.0f)
      )
    )

  test("+ scalar returns new Float matrix and leaves source unchanged"):
    val mat = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f, 3.0f),
      Array[Float](4.0f, 5.0f, 6.0f)
    )

    val result = mat + 1.5f

    assertFloatMatrixEquals(
      result,
      Matrix.fromRows[Float](
        Array[Float](2.5f, 3.5f, 4.5f),
        Array[Float](5.5f, 6.5f, 7.5f)
      )
    )
    assertFloatMatrixEquals(
      mat,
      Matrix.fromRows[Float](
        Array[Float](1.0f, 2.0f, 3.0f),
        Array[Float](4.0f, 5.0f, 6.0f)
      )
    )

  test("left scalar Float operators delegate to matrix implementations"):
    val mat = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f),
      Array[Float](3.0f, 4.0f)
    )

    assertFloatMatrixEquals(
      2.0f * mat,
      Matrix.fromRows[Float](
        Array[Float](2.0f, 4.0f),
        Array[Float](6.0f, 8.0f)
      )
    )

    assertFloatMatrixEquals(
      1.5f + mat,
      Matrix.fromRows[Float](
        Array[Float](2.5f, 3.5f),
        Array[Float](4.5f, 5.5f)
      )
    )

  test("Float matrix-vector multiply supports alpha scaling"):
    val mat = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f, 3.0f),
      Array[Float](4.0f, 5.0f, 6.0f)
    )

    val result = mat.*(Array[Float](1.0f, 0.5f, -1.0f), alpha = 2.0f, beta = 0.0f)

    assertFloatVecEquals(result, Array[Float](-2.0f, 1.0f))

  test("matmulInPlace! on dense Float matrices applies alpha and beta"):
    val a = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f),
      Array[Float](3.0f, 4.0f)
    )
    val b = Matrix.fromRows[Float](
      Array[Float](5.0f, 6.0f),
      Array[Float](7.0f, 8.0f)
    )
    val out = Matrix.fromRows[Float](
      Array[Float](1.0f, 0.0f),
      Array[Float](0.0f, 1.0f)
    )

    a.`matmulInPlace!`(b, out, alpha = 0.5f, beta = 2.0f)

    assertFloatMatrixEquals(
      out,
      Matrix.fromRows[Float](
        Array[Float](11.5f, 11.0f),
        Array[Float](21.5f, 27.0f)
      )
    )

  test("matmulInPlace! supports offset Float views"):
    val base = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f, 3.0f, 4.0f),
      Array[Float](5.0f, 6.0f, 7.0f, 8.0f),
      Array[Float](9.0f, 10.0f, 11.0f, 12.0f),
      Array[Float](13.0f, 14.0f, 15.0f, 16.0f)
    )

    val a = base(Range.Inclusive(1, 2, 1), Range.Inclusive(1, 2, 1))
    val b = base(Range.Inclusive(1, 2, 1), Range.Inclusive(0, 1, 1))
    val out = Matrix.zeros[Float]((2, 2))

    a.`matmulInPlace!`(b, out, alpha = 1.0f, beta = 0.0f)

    assertFloatMatrixEquals(
      out,
      Matrix.fromRows[Float](
        Array[Float](93.0f, 106.0f),
        Array[Float](149.0f, 170.0f)
      )
    )

  test("- on two dense contiguous Float matrices returns correct result"):
    val a = Matrix.fromRows[Float](
      Array[Float](5.0f, 8.0f, 3.0f),
      Array[Float](1.0f, 6.0f, 9.0f)
    )
    val b = Matrix.fromRows[Float](
      Array[Float](2.0f, 3.0f, 1.0f),
      Array[Float](4.0f, 1.0f, 7.0f)
    )
    val result = a - b
    assertFloatMatrixEquals(
      result,
      Matrix.fromRows[Float](
        Array[Float](3.0f, 5.0f, 2.0f),
        Array[Float](-3.0f, 5.0f, 2.0f)
      )
    )

  test("- works when both operands are submatrix views"):
    // 4x4 backing matrix, take a 2x2 slice from each
    val big = Matrix.fromRows[Float](
      Array[Float](10.0f, 20.0f, 30.0f, 40.0f),
      Array[Float](50.0f, 60.0f, 70.0f, 80.0f),
      Array[Float](90.0f, 100.0f, 110.0f, 120.0f),
      Array[Float](130.0f, 140.0f, 150.0f, 160.0f)
    )
    val viewA = big(0 until 2, 0 until 2) // [[10,20],[50,60]]
    val viewB = big(2 until 4, 2 until 4) // [[110,120],[150,160]]

    // views have offsets and strides, not contiguous
    val result = viewA - viewB
    assertEquals(result.rows, 2)
    assertEquals(result.cols, 2)
    assertFloatMatrixEquals(
      result,
      Matrix.fromRows[Float](
        Array[Float](-100.0f, -100.0f),
        Array[Float](-100.0f, -100.0f)
      )
    )

  test("- works when one operand is a view and the other is dense"):
    val big = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f, 3.0f),
      Array[Float](4.0f, 5.0f, 6.0f),
      Array[Float](7.0f, 8.0f, 9.0f)
    )
    val view = big(0 until 2, 1 until 3) // [[2,3],[5,6]]
    val dense = Matrix.fromRows[Float](
      Array[Float](1.0f, 1.0f),
      Array[Float](2.0f, 2.0f)
    )
    val result = view - dense
    assertFloatMatrixEquals(
      result,
      Matrix.fromRows[Float](
        Array[Float](1.0f, 2.0f),
        Array[Float](3.0f, 4.0f)
      )
    )

end FloatMatrixJvmSuite
