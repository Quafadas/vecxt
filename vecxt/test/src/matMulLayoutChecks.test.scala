package vecxt

import all.*
import munit.FunSuite
import narr.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

/** Here we test the matrix multiplication with different memory layouts. Col Major * Col Major Row Major * Row Major
  * Col Major * Row Major Row Major * Col Major
  *
  * If all of these work out, then we can hope that we are feeding BLAS the correct parameters.
  */
class DifferentMemoryLayoutTests extends FunSuite:

  /** I don't think this can work
    */
  // test("offsets".only) {
  //   def makeMat = Matrix[Double](NArray.tabulate[Double](9)(_.toDouble + 1), 3, 3, 1, 3, 0)
  //   val r = Array(1,2)
  //   val mat = makeMat(r, r)
  //   val mat2 = makeMat(r, r)

  //   println(s"mat.rowStride: ${mat.rowStride}, mat.colStride: ${mat.colStride}, mat.offset: ${mat.offset} rows: ${mat.rows}, cols: ${mat.cols}")

  //   val mat3 = Matrix.fromRows(
  //     NArray(5.0, 6.0),
  //     NArray(8.0, 9.0)
  //   )

  //   println(mat.printMat)

  //   println(mat3.printMat)
  //   println((mat3 @@ mat3).printMat)

  //   println((mat @@ mat2).printMat)
  // }

  test("scalars in matmul") {
    def makeMat = Matrix[Double](NArray.tabulate[Double](9)(_.toDouble + 1), 3, 3, 3, 1, 0)
    val eye = Matrix.eye[Double](3)
    val eye2 = eye * 2.0

    assertMatrixEquals(makeMat @@ eye, makeMat)

    assertMatrixEquals(eye.matmul(makeMat, 2.0, 0.0), makeMat * 2.0)

    val outMat = Matrix.eye[Double](3)

    makeMat.`matmulInPlace!`(eye, outMat, 2.0, 2.0)

    assertMatrixEquals(outMat, (makeMat * 2.0) + eye2)

  }

  test("matmul col major * row major") {
    val matRow = Matrix[Double](NArray.tabulate[Double](9)(_.toDouble + 1), 3, 3, 3, 1, 0)
    val matCol = Matrix[Double](NArray.tabulate[Double](9)(_.toDouble + 1), 3, 3, 1, 3, 0)

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
        NArray(1.0, 2, 3, 4),
        NArray(5.0, 6, 7, 8),
        NArray(9.0, 10, 11, 12),
        NArray(13.0, 14, 15, 16)
    )
    val mat2 = Matrix.fromRows(
        NArray(1.0, 2, 3, 4),
        NArray(5.0, 6, 7, 8),
        NArray(9.0, 10, 11, 12),
        NArray(13.0, 14, 15, 16),
        NArray(1.0, 2, 3, 4)
    )

    val subMat = Range.Inclusive(1, 2, 1)

    // Zero copy submatrix
    val zeroCopy = mat1(subMat, subMat) // Essentially a "view" of the original matrix
    val zeroCopy2 = mat2(subMat, subMat)

    val newMat = zeroCopy @@ zeroCopy2

    assertEqualsDouble(newMat(0,0), 6 * 6 + 7 * 10, 0.000001)
    assertEqualsDouble(newMat(1,0), 10 * 6 + 10 * 11, 0.000001)
    assertEqualsDouble(newMat(1,1), 10 * 7 + 11 * 11, 0.000001)
    assertEqualsDouble(newMat(0,1), 7 * 6 + 7 * 11, 0.000001)


  }

  // test("matmul different dimensions"){
  //   val mat1 = Matrix[Double](NArray.tabulate[Double](6)(_.toDouble + 1), 3, 2, 1, 3, 0)
  //   val mat2 = Matrix[Double](NArray.tabulate[Double](9)(_.toDouble + 1), 3, 3, 1, 3, 0)

  //   val matMul = mat2 @@ mat1

  //   println(mat1.printMat)
  //   println(mat2.printMat)
  //   println(matMul.printMat)

  //   assertEqualsDouble(matMul(0,0), 1 * 1 + 2 * 2 + 3 * 3, 0.0001)
  //   assertEqualsDouble(matMul(0,2), 1 * 7 + 2 * 8 + 3 * 9, 0.0001)
  //   assertEqualsDouble(matMul(1,1), 4 * 4 + 5 * 5 + 6 * 6, 0.0001)

  // }

end DifferentMemoryLayoutTests
