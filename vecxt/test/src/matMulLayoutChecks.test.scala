package vecxt

import all.*
import munit.FunSuite

/** Here we test the matrix multiplication with different memory layouts. Col Major * Col Major Row Major * Row Major
  * Col Major * Row Major Row Major * Col Major
  *
  * If all of these work out, then we can hope that we are feeding BLAS the correct parameters.
  */
class DifferentMemoryLayoutTests extends FunSuite:

  /** I don't think this can work
    */
  // test("offsets".only) {
  //   def makeMat = Matrix[Double](Array.tabulate[Double](9)(_.toDouble + 1), 3, 3, 1, 3, 0)
  //   val r = Array(1,2)
  //   val mat = makeMat(r, r)
  //   val mat2 = makeMat(r, r)

  //   println(s"mat.rowStride: ${mat.rowStride}, mat.colStride: ${mat.colStride}, mat.offset: ${mat.offset} rows: ${mat.rows}, cols: ${mat.cols}")

  //   val mat3 = Matrix.fromRows(
  //     Array(5.0, 6.0),
  //     Array(8.0, 9.0)
  //   )

  //   println(mat.printMat)

  //   println(mat3.printMat)
  //   println((mat3 @@ mat3).printMat)

  //   println((mat @@ mat2).printMat)
  // }

  test("scalars in matmul") {
    def makeMat = Matrix[Double](Array.tabulate[Double](9)(_.toDouble + 1), 3, 3, 3, 1, 0)
    val eye = Matrix.eye[Double](3)
    val eye2 = eye * 2.0

    assertMatrixEquals(makeMat @@ eye, makeMat)

    assertMatrixEquals(eye.matmul(makeMat, 2.0, 0.0), makeMat * 2.0)

    val outMat = Matrix.eye[Double](3)

    makeMat.`matmulInPlace!`(eye, outMat, 2.0, 2.0)

    assertMatrixEquals(outMat, (makeMat * 2.0) + eye2)

  }

  test("matmul col major * row major") {
    val matRow = Matrix[Double](Array.tabulate[Double](9)(_.toDouble + 1), 3, 3, 3, 1, 0)
    val matCol = Matrix[Double](Array.tabulate[Double](9)(_.toDouble + 1), 3, 3, 1, 3, 0)

    // def assertMatMulResult(mat: Matrix[Double]): Unit =
    //   assertEqualsDouble(mat(0,0), 1 * 1 + 2 * 2 + 3 * 3, 0.0001)
    //   assertEqualsDouble(mat(0,2), 1 * 7 + 2 * 8 + 3 * 9, 0.0001)
    //   assertEqualsDouble(mat(1,1), 4 * 4 + 5 * 5 + 6 * 6, 0.0001)
    //   assertEqualsDouble(mat(2,0), 7 * 1 + 8 * 2 + 9 * 3, 0.0001)

    val mat = matCol @@ matCol

    // println("mat1")
    // println(matCol.printMat)
    // println("mat2---")
    // println(matCol.printMat )
    // println("result---")
    // println(mat.printMat)

    assertEqualsDouble(mat(0, 0), 1 * 1 + 4 * 2 + 3 * 7, 0.0001)
    assertEqualsDouble(mat(0, 2), 1 * 7 + 4 * 8 + 7 * 9, 0.0001)
    assertEqualsDouble(mat(1, 1), 4 * 2 + 5 * 5 + 8 * 6, 0.0001)
    assertEqualsDouble(mat(2, 0), 3 * 1 + 6 * 2 + 9 * 3, 0.0001)

    val mat2 = matCol @@ matRow
    // println(matCol.printMat )
    // println("---")
    // println(matRow.printMat )
    // println("---")
    // println(mat2.printMat)
    assertEqualsDouble(mat2(0, 0), 1 * 1 + 4 * 4 + 7 * 7, 0.0001)
    assertEqualsDouble(mat2(0, 2), 1 * 3 + 4 * 6 + 7 * 9, 0.0001)
    assertEqualsDouble(mat2(1, 1), 2 * 2 + 5 * 5 + 8 * 8, 0.0001)
    assertEqualsDouble(mat2(2, 0), 3 * 1 + 6 * 4 + 9 * 7, 0.0001)

    val mat3 = matRow @@ matRow
    // println(matRow.printMat )
    // println("---")
    // println(matRow.printMat )
    // println("---")
    // println(mat3.printMat)
    assertEqualsDouble(mat3(0, 0), 1 * 1 + 2 * 4 + 7 * 3, 0.0001)
    assertEqualsDouble(mat3(0, 2), 1 * 3 + 2 * 6 + 3 * 9, 0.0001)
    assertEqualsDouble(mat3(1, 1), 4 * 2 + 5 * 5 + 6 * 8, 0.0001)
    assertEqualsDouble(mat3(2, 0), 7 * 1 + 8 * 4 + 9 * 7, 0.0001)

    val mat4 = matRow @@ matCol
    // println(matRow.printMat )
    // println("---")
    // println(matCol.printMat )
    // println("---")
    // println(mat4.printMat)
    assertEqualsDouble(mat4(0, 0), 1 * 1 + 2 * 2 + 3 * 3, 0.0001)
    assertEqualsDouble(mat4(0, 2), 1 * 7 + 2 * 8 + 3 * 9, 0.0001)
    assertEqualsDouble(mat4(1, 1), 4 * 4 + 5 * 5 + 6 * 6, 0.0001)
    assertEqualsDouble(mat4(2, 0), 7 * 1 + 8 * 2 + 9 * 3, 0.0001)

  }

  test("Col major with offset") {
    val mat1 = Matrix.fromRows(
      Array(1.0, 2, 3, 4),
      Array(5.0, 6, 7, 8),
      Array(9.0, 10, 11, 12),
      Array(13.0, 14, 15, 16)
    )
    val mat2 = Matrix.fromRows(
      Array(1.0, 2, 3, 4),
      Array(5.0, 6, 7, 8),
      Array(9.0, 10, 11, 12),
      Array(13.0, 14, 15, 16),
      Array(1.0, 2, 3, 4)
    )

    val subMat = Range.Inclusive(1, 2, 1)

    // Zero copy submatrix
    val zeroCopy = mat1(subMat, subMat) // Essentially a "view" of the original matrix
    val zeroCopy2 = mat2(subMat, subMat)

    val newMat = zeroCopy @@ zeroCopy2

    assertEqualsDouble(newMat(0, 0), 6 * 6 + 7 * 10, 0.000001)
    assertEqualsDouble(newMat(1, 0), 10 * 6 + 10 * 11, 0.000001)
    assertEqualsDouble(newMat(1, 1), 10 * 7 + 11 * 11, 0.000001)
    assertEqualsDouble(newMat(0, 1), 7 * 6 + 7 * 11, 0.000001)

    val subMat3 = Range.Inclusive(1, 3, 1)

    val hardCopy = mat1(subMat3, subMat)

    val checkAgainst = hardCopy.deepCopy @@ hardCopy.transpose.deepCopy

    val matMul = hardCopy @@ hardCopy.transpose

    // println("to check")
    // println(checkAgainst.printMat)
    // println("against")
    // println(matMul.printMat)

    for i <- 0 until checkAgainst.rows do
      for j <- 0 until checkAgainst.cols do assertEqualsDouble(checkAgainst(i, j), matMul(i, j), 0.00001, (i, j))
    end for

    val checkAgainst2 = hardCopy.transpose.deepCopy @@ hardCopy.deepCopy
    val matMul2 = hardCopy.transpose @@ hardCopy

    // println("to check")
    // println(checkAgainst2.printMat)

    // println("against")
    // println(matMul2.printMat)

    // println(checkAgainst2.layoutString)
    // println(matMul2.layoutString)

    for i <- 0 until checkAgainst2.rows do
      for j <- 0 until checkAgainst2.cols do assertEqualsDouble(checkAgainst2(i, j), matMul2(i, j), 0.00001, (i, j))
    end for

    // Non-square offset views: `lda`/`ldb` from a strided (non-zero-offset) view, exercised at a shape where
    // m, k, n are pairwise distinct, not just "some view happens to be non-square" as above.
    // view1 = mat1 rows 0..2, cols 1..2 -> 3x2: [[2,3],[6,7],[10,11]]
    // view2 = mat2 rows 1..2, cols 0..3 -> 2x4: [[5,6,7,8],[9,10,11,12]]
    val view1 = mat1(Range.Inclusive(0, 2, 1), Range.Inclusive(1, 2, 1))
    val view2 = mat2(Range.Inclusive(1, 2, 1), Range.Inclusive(0, 3, 1))
    val viewMul = view1 @@ view2

    assertMatrixEquals(
      viewMul,
      Matrix.fromRows(
        Array(37.0, 42.0, 47.0, 52.0),
        Array(93.0, 106.0, 119.0, 132.0),
        Array(149.0, 170.0, 191.0, 212.0)
      )
    )
  }

  // ─── Non-square matmul, all layout combinations ───────────────────────────────────────────────────────────────
  // The tests above only ever multiply square (3x3) or near-square (2x2) matrices, where m == n == k (or m == n
  // with only k differing). On a square input, lda/ldb/ldc/rows/cols/the inner dimension are all the same number,
  // so an incorrect leading dimension or transpose flag is indistinguishable from a correct one — exactly the
  // regime where BLAS parameter bugs hide. Below, A is 3x2 and B is 2x4, so m=3, k=2, n=4 are pairwise distinct.

  test("matmul non-square, all layout combinations") {
    // A (3x2), logical: [[1,2],[3,4],[5,6]]
    val aColMajor = Matrix[Double](Array(1.0, 3.0, 5.0, 2.0, 4.0, 6.0), 3, 2, 1, 3, 0)
    val aRowMajor = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 3, 2, 2, 1, 0)

    // B (2x4), logical: [[1,2,3,4],[5,6,7,8]]
    val bColMajor = Matrix[Double](Array(1.0, 5.0, 2.0, 6.0, 3.0, 7.0, 4.0, 8.0), 2, 4, 1, 2, 0)
    val bRowMajor = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), 2, 4, 4, 1, 0)

    // A @@ B, hand-computed (3x4): row i of A dotted with each column of B.
    val expected = Matrix.fromRows(
      Array(11.0, 14.0, 17.0, 20.0),
      Array(23.0, 30.0, 37.0, 44.0),
      Array(35.0, 46.0, 57.0, 68.0)
    )

    assertMatrixEquals(aColMajor @@ bColMajor, expected)
    assertMatrixEquals(aColMajor @@ bRowMajor, expected)
    assertMatrixEquals(aRowMajor @@ bColMajor, expected)
    assertMatrixEquals(aRowMajor @@ bRowMajor, expected)
  }

  test("matmul non-square, m > n") {
    // A (4x3), logical: [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
    val a = Matrix.fromRows(
      Array(1.0, 2.0, 3.0),
      Array(4.0, 5.0, 6.0),
      Array(7.0, 8.0, 9.0),
      Array(10.0, 11.0, 12.0)
    )
    // B (3x2), logical: [[1,0],[0,1],[1,1]]
    val b = Matrix.fromRows(
      Array(1.0, 0.0),
      Array(0.0, 1.0),
      Array(1.0, 1.0)
    )

    // A @@ B, hand-computed (4x2). m=4, k=3, n=2: a rows/cols transposition (e.g. swapping ldc for lda, or m for
    // n) cannot silently pass both this and the m < n case above, since here m > n instead.
    val expected = Matrix.fromRows(
      Array(4.0, 5.0),
      Array(10.0, 11.0),
      Array(16.0, 17.0),
      Array(22.0, 23.0)
    )

    assertMatrixEquals(a @@ b, expected)
  }

  test("matmul non-square, beta accumulation") {
    // Same A/B as "matmul non-square, all layout combinations" (m=3, k=2, n=4, pairwise distinct), but exercising
    // matmulInPlace! directly with alpha != 1 and beta != 0, so the accumulation path is also checked away from a
    // square shape.
    val a = Matrix.fromRows(Array(1.0, 2.0), Array(3.0, 4.0), Array(5.0, 6.0))
    val b = Matrix.fromRows(Array(1.0, 2.0, 3.0, 4.0), Array(5.0, 6.0, 7.0, 8.0))
    val out = Matrix.fromRows(
      Array(1.0, 1.0, 1.0, 1.0),
      Array(1.0, 1.0, 1.0, 1.0),
      Array(1.0, 1.0, 1.0, 1.0)
    )

    a.`matmulInPlace!`(b, out, 2.0, 3.0)

    // hand-computed: alpha * (a @@ b) + beta * outBefore
    // a @@ b = [[11,14,17,20],[23,30,37,44],[35,46,57,68]]; 2 * that + 3 * (all-ones)
    assertMatrixEquals(
      out,
      Matrix.fromRows(
        Array(25.0, 31.0, 37.0, 43.0),
        Array(49.0, 63.0, 77.0, 91.0),
        Array(73.0, 95.0, 117.0, 139.0)
      )
    )
  }

  // test("matmul different dimensions"){
  //   val mat1 = Matrix[Double](Array.tabulate[Double](6)(_.toDouble + 1), 3, 2, 1, 3, 0)
  //   val mat2 = Matrix[Double](Array.tabulate[Double](9)(_.toDouble + 1), 3, 3, 1, 3, 0)

  //   val matMul = mat2 @@ mat1

  //   println(mat1.printMat)
  //   println(mat2.printMat)
  //   println(matMul.printMat)

  //   assertEqualsDouble(matMul(0,0), 1 * 1 + 2 * 2 + 3 * 3, 0.0001)
  //   assertEqualsDouble(matMul(0,2), 1 * 7 + 2 * 8 + 3 * 9, 0.0001)
  //   assertEqualsDouble(matMul(1,1), 4 * 4 + 5 * 5 + 6 * 6, 0.0001)

  // }

  // ─── `matmulInPlace!` guard precedence + output-matrix validation ─────────────────────────────────────────────
  // The layout guard used to parse as `m.rowStride == 1 || (m.colStride == 1 && b.rowStride == 1) || b.colStride ==
  // 1` (`&&` binds tighter than `||`), so any `m` with `rowStride == 1` short-circuited the whole guard to `true`
  // regardless of `b` — including a `b` with neither stride equal to 1, which `dgemm` cannot actually address
  // correctly with a single leading-dimension parameter. Separately, `c` was never checked at all: it's assumed
  // dense column-major (`ldc = m.rows`) and is both written, and — whenever `beta != 0` — read, on that assumption.

  test("doubly-strided b with unit-rowStride m throws rather than returning numbers") {
    // m: rowStride == 1, but not dense (colStride (3) != rows (2)) so it can't take the fully-dense BLAS path.
    val m = Matrix[Double](Array.tabulate[Double](6)(_.toDouble + 1), 2, 2, 1, 3, 0)
    // b: doubly strided — neither rowStride nor colStride is 1.
    val b = Matrix[Double](Array.tabulate[Double](10)(_.toDouble + 1), 2, 2, 2, 5, 0)

    intercept[UnsupportedLayoutException] {
      m @@ b
    }
  }

  test("matmulInPlace! throws when c is row-major instead of column-major") {
    val a = Matrix.fromRows(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0)) // 2x3
    val b = Matrix.fromRows(Array(1.0, 2.0), Array(3.0, 4.0), Array(5.0, 6.0)) // 3x2
    val cRowMajor = Matrix[Double](Array.ofDim[Double](4), 2, 2, 2, 1, 0) // correctly shaped, but row-major

    intercept[UnsupportedLayoutException] {
      a.`matmulInPlace!`(b, cRowMajor, 1.0, 0.0)
    }
  }

  test("matmulInPlace! throws MatrixDimensionMismatch when c is the wrong shape") {
    val a = Matrix.fromRows(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0)) // 2x3
    val b = Matrix.fromRows(Array(1.0, 2.0), Array(3.0, 4.0), Array(5.0, 6.0)) // 3x2
    val wrongSizeC = Matrix.zeros[Double]((3, 3)) // should be (2, 2) == (a.rows, b.cols)

    intercept[MatrixDimensionMismatch] {
      a.`matmulInPlace!`(b, wrongSizeC, 1.0, 0.0)
    }
  }

  test("scalars in matmul, non-square") {
    val m = Matrix.fromRows(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0)) // 2x3
    val b = Matrix.fromRows(Array(1.0, 0.0), Array(0.0, 1.0), Array(1.0, 1.0)) // 3x2
    val out = Matrix.fromRows(Array(1.0, 0.0), Array(0.0, 1.0)) // 2x2, accumulated into via beta

    m.`matmulInPlace!`(b, out, 2.0, 2.0)

    // hand-computed: m @@ b = [[4, 5], [10, 11]]; alpha * (m @@ b) + beta * outBefore
    assertMatrixEquals(
      out,
      Matrix.fromRows(Array(10.0, 10.0), Array(20.0, 24.0))
    )
  }

end DifferentMemoryLayoutTests
