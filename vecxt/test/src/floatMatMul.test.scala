package vecxt

import all.*
import munit.FunSuite

/** `Matrix[Float] @@ Matrix[Float]`, `matmul`, and `matmulInPlace!` across every memory layout. Shared rather than
  * JVM-only because the operation now exists on every platform — netlib `sgemm` on the JVM, `cblas_sgemm` on Native,
  * and the stdlib `sgemm` shim on JS — following the same layout rules as [[DifferentMemoryLayoutTests]] does for
  * `Double`. Previously `matmulInPlace!`, `matmul`, and `@@` existed only on the JVM for `Float`.
  */
class FloatMatMulSuite extends FunSuite:

  test("scalars in Float matmul") {
    def makeMat = Matrix[Float](Array.tabulate[Float](9)(_.toFloat + 1), 3, 3, 3, 1, 0)
    val eye = Matrix.eye[Float](3)

    assertMatrixEquals(makeMat @@ eye, makeMat)

    val doubledMat = Matrix[Float](Array.tabulate[Float](9)(idx => (idx.toFloat + 1) * 2.0f), 3, 3, 3, 1, 0)
    assertMatrixEquals(eye.matmul(makeMat, 2.0f, 0.0f), doubledMat)

    val outMat = Matrix.eye[Float](3)

    makeMat.`matmulInPlace!`(eye, outMat, 2.0f, 2.0f)

    // hand-computed: alpha * (makeMat @@ eye) + beta * outBefore, outBefore == eye
    val expected = Matrix[Float](
      Array.tabulate[Float](9) { idx =>
        val row = idx % 3
        val col = idx / 3
        doubledMat(row, col) + (if row == col then 2.0f else 0.0f)
      },
      3,
      3,
      1,
      3,
      0
    )
    assertMatrixEquals(outMat, expected)
  }

  test("Float matmul col major * row major, all combinations") {
    val matRow = Matrix[Float](Array.tabulate[Float](9)(_.toFloat + 1), 3, 3, 3, 1, 0)
    val matCol = Matrix[Float](Array.tabulate[Float](9)(_.toFloat + 1), 3, 3, 1, 3, 0)

    val mat = matCol @@ matCol
    assertEqualsDouble(mat(0, 0).toDouble, 1 * 1 + 4 * 2 + 3 * 7, 0.0001)
    assertEqualsDouble(mat(0, 2).toDouble, 1 * 7 + 4 * 8 + 7 * 9, 0.0001)
    assertEqualsDouble(mat(1, 1).toDouble, 4 * 2 + 5 * 5 + 8 * 6, 0.0001)
    assertEqualsDouble(mat(2, 0).toDouble, 3 * 1 + 6 * 2 + 9 * 3, 0.0001)

    val mat2 = matCol @@ matRow
    assertEqualsDouble(mat2(0, 0).toDouble, 1 * 1 + 4 * 4 + 7 * 7, 0.0001)
    assertEqualsDouble(mat2(0, 2).toDouble, 1 * 3 + 4 * 6 + 7 * 9, 0.0001)
    assertEqualsDouble(mat2(1, 1).toDouble, 2 * 2 + 5 * 5 + 8 * 8, 0.0001)
    assertEqualsDouble(mat2(2, 0).toDouble, 3 * 1 + 6 * 4 + 9 * 7, 0.0001)

    val mat3 = matRow @@ matRow
    assertEqualsDouble(mat3(0, 0).toDouble, 1 * 1 + 2 * 4 + 7 * 3, 0.0001)
    assertEqualsDouble(mat3(0, 2).toDouble, 1 * 3 + 2 * 6 + 3 * 9, 0.0001)
    assertEqualsDouble(mat3(1, 1).toDouble, 4 * 2 + 5 * 5 + 6 * 8, 0.0001)
    assertEqualsDouble(mat3(2, 0).toDouble, 7 * 1 + 8 * 4 + 9 * 7, 0.0001)

    val mat4 = matRow @@ matCol
    assertEqualsDouble(mat4(0, 0).toDouble, 1 * 1 + 2 * 2 + 3 * 3, 0.0001)
    assertEqualsDouble(mat4(0, 2).toDouble, 1 * 7 + 2 * 8 + 3 * 9, 0.0001)
    assertEqualsDouble(mat4(1, 1).toDouble, 4 * 4 + 5 * 5 + 6 * 6, 0.0001)
    assertEqualsDouble(mat4(2, 0).toDouble, 7 * 1 + 8 * 2 + 9 * 3, 0.0001)
  }

  test("Float matmul with offset (submatrix) views") {
    val mat1 = Matrix.fromRows(
      Array(1.0f, 2.0f, 3.0f, 4.0f),
      Array(5.0f, 6.0f, 7.0f, 8.0f),
      Array(9.0f, 10.0f, 11.0f, 12.0f),
      Array(13.0f, 14.0f, 15.0f, 16.0f)
    )
    val mat2 = Matrix.fromRows(
      Array(1.0f, 2.0f, 3.0f, 4.0f),
      Array(5.0f, 6.0f, 7.0f, 8.0f),
      Array(9.0f, 10.0f, 11.0f, 12.0f),
      Array(13.0f, 14.0f, 15.0f, 16.0f),
      Array(1.0f, 2.0f, 3.0f, 4.0f)
    )

    val subMat = Range.Inclusive(1, 2, 1)

    val zeroCopy = mat1(subMat, subMat)
    val zeroCopy2 = mat2(subMat, subMat)

    val newMat = zeroCopy @@ zeroCopy2

    assertEqualsDouble(newMat(0, 0).toDouble, 6 * 6 + 7 * 10, 0.000001)
    assertEqualsDouble(newMat(1, 0).toDouble, 10 * 6 + 10 * 11, 0.000001)
    assertEqualsDouble(newMat(1, 1).toDouble, 10 * 7 + 11 * 11, 0.000001)
    assertEqualsDouble(newMat(0, 1).toDouble, 7 * 6 + 7 * 11, 0.000001)

    val view1 = mat1(Range.Inclusive(0, 2, 1), Range.Inclusive(1, 2, 1))
    val view2 = mat2(Range.Inclusive(1, 2, 1), Range.Inclusive(0, 3, 1))
    val viewMul = view1 @@ view2

    assertMatrixEquals(
      viewMul,
      Matrix.fromRows(
        Array(37.0f, 42.0f, 47.0f, 52.0f),
        Array(93.0f, 106.0f, 119.0f, 132.0f),
        Array(149.0f, 170.0f, 191.0f, 212.0f)
      )
    )
  }

  test("Float matmul non-square, all layout combinations") {
    // A (3x2), logical: [[1,2],[3,4],[5,6]]
    val aColMajor = Matrix[Float](Array(1.0f, 3.0f, 5.0f, 2.0f, 4.0f, 6.0f), 3, 2, 1, 3, 0)
    val aRowMajor = Matrix[Float](Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), 3, 2, 2, 1, 0)

    // B (2x4), logical: [[1,2,3,4],[5,6,7,8]]
    val bColMajor = Matrix[Float](Array(1.0f, 5.0f, 2.0f, 6.0f, 3.0f, 7.0f, 4.0f, 8.0f), 2, 4, 1, 2, 0)
    val bRowMajor = Matrix[Float](Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f), 2, 4, 4, 1, 0)

    val expected = Matrix.fromRows(
      Array(11.0f, 14.0f, 17.0f, 20.0f),
      Array(23.0f, 30.0f, 37.0f, 44.0f),
      Array(35.0f, 46.0f, 57.0f, 68.0f)
    )

    assertMatrixEquals(aColMajor @@ bColMajor, expected)
    assertMatrixEquals(aColMajor @@ bRowMajor, expected)
    assertMatrixEquals(aRowMajor @@ bColMajor, expected)
    assertMatrixEquals(aRowMajor @@ bRowMajor, expected)
  }

  test("Float matmul non-square, beta accumulation") {
    val a = Matrix.fromRows(Array(1.0f, 2.0f), Array(3.0f, 4.0f), Array(5.0f, 6.0f))
    val b = Matrix.fromRows(Array(1.0f, 2.0f, 3.0f, 4.0f), Array(5.0f, 6.0f, 7.0f, 8.0f))
    val out = Matrix.fromRows(
      Array(1.0f, 1.0f, 1.0f, 1.0f),
      Array(1.0f, 1.0f, 1.0f, 1.0f),
      Array(1.0f, 1.0f, 1.0f, 1.0f)
    )

    a.`matmulInPlace!`(b, out, 2.0f, 3.0f)

    // hand-computed: alpha * (a @@ b) + beta * outBefore
    assertMatrixEquals(
      out,
      Matrix.fromRows(
        Array(25.0f, 31.0f, 37.0f, 43.0f),
        Array(49.0f, 63.0f, 77.0f, 91.0f),
        Array(73.0f, 95.0f, 117.0f, 139.0f)
      )
    )
  }

  test("Float matmul rejects doubly-strided b with unit-rowStride m") {
    val m = Matrix[Float](Array.tabulate[Float](6)(_.toFloat + 1), 2, 2, 1, 3, 0)
    val b = Matrix[Float](Array.tabulate[Float](10)(_.toFloat + 1), 2, 2, 2, 5, 0)

    intercept[UnsupportedLayoutException] {
      m @@ b
    }
  }

  test("Float matmul rejects a broadcast operand rather than handing BLAS lda = 0") {
    val broadcast = Matrix[Float](Array(1.0f, 2.0f), 2, 2, 1, 0, 0)
    val dense = Matrix.fromRows(Array(1.0f, 0.0f), Array(0.0f, 1.0f))

    assert(broadcast.rowStride == 1 && broadcast.colStride == 0, "fixture must be the half-satisfying case")
    intercept[UnsupportedLayoutException](broadcast @@ dense)
    intercept[UnsupportedLayoutException](dense @@ broadcast)
  }

  test("Float matmulInPlace! throws when c is row-major instead of column-major") {
    val a = Matrix.fromRows(Array(1.0f, 2.0f, 3.0f), Array(4.0f, 5.0f, 6.0f)) // 2x3
    val b = Matrix.fromRows(Array(1.0f, 2.0f), Array(3.0f, 4.0f), Array(5.0f, 6.0f)) // 3x2
    val cRowMajor = Matrix[Float](Array.ofDim[Float](4), 2, 2, 2, 1, 0) // correctly shaped, but row-major

    intercept[UnsupportedLayoutException] {
      a.`matmulInPlace!`(b, cRowMajor, 1.0f, 0.0f)
    }
  }

  test("Float matmulInPlace! throws MatrixDimensionMismatch when c is the wrong shape") {
    val a = Matrix.fromRows(Array(1.0f, 2.0f, 3.0f), Array(4.0f, 5.0f, 6.0f)) // 2x3
    val b = Matrix.fromRows(Array(1.0f, 2.0f), Array(3.0f, 4.0f), Array(5.0f, 6.0f)) // 3x2
    val wrongSizeC = Matrix.zeros[Float]((3, 3)) // should be (2, 2) == (a.rows, b.cols)

    intercept[MatrixDimensionMismatch] {
      a.`matmulInPlace!`(b, wrongSizeC, 1.0f, 0.0f)
    }
  }

  test("Float matmulInPlace! throws for unsupported general layouts") {
    val left = Matrix[Float](
      Array[Float](1.0f, 90.0f, 2.0f, 91.0f, 92.0f, 3.0f, 93.0f, 4.0f),
      2,
      2,
      2,
      5,
      0
    )
    val right = Matrix[Float](
      Array[Float](5.0f, 80.0f, 6.0f, 81.0f, 82.0f, 7.0f, 83.0f, 8.0f),
      2,
      2,
      2,
      5,
      0
    )
    val out = Matrix.zeros[Float]((2, 2))

    intercept[UnsupportedLayoutException] {
      left.`matmulInPlace!`(right, out, alpha = 1.0f, beta = 0.0f)
    }
  }

end FloatMatMulSuite
