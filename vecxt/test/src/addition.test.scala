package vecxt

import all.*
import munit.FunSuite
import narr.*
import javax.print.attribute.standard.MediaSize.NA

class MatrixAdditionTest extends FunSuite:
  import vecxt.BoundsCheck.DoBoundsCheck.yes

  def mat9 =
    Matrix[Double](NArray.tabulate[Double](9)(_.toDouble + 1), 3, 3, 3, 1, 0)

  def mat36 =
    Matrix[Double](NArray.tabulate[Double](36)(_.toDouble), 6, 6, 6, 1, 0)

  test("scalar addition with strides and offset is zero copy"):
    val mat1 = mat9
    var arr = mat1.raw

    mat1 += 10.0

    assertMatrixEquals(
      mat1,
      Matrix.fromRows(
        NArray[Double](11.0, 12.0, 13.0),
        NArray[Double](14.0, 15.0, 16.0),
        NArray[Double](17.0, 18.0, 19.0)
      )
    )

    mat1.submatrix(0 to 1, 0 to 1) += 1.0

    assertMatrixEquals(
      mat1,
      Matrix.fromRows(
        NArray[Double](12.0, 13.0, 13.0),
        NArray[Double](15.0, 16.0, 16.0),
        NArray[Double](17.0, 18.0, 19.0)
      )
    )

    mat1.submatrix(1 to 2, 1 to 2) += 1.0
    assertMatrixEquals(
      mat1,
      Matrix.fromRows(
        NArray[Double](12.0, 13.0, 13.0),
        NArray[Double](15.0, 17.0, 17.0),
        NArray[Double](17.0, 19.0, 20.0)
      )
    )
    // Check reference equality. All operations were zero-copy
    assert(arr == mat1.raw)

  test("scalar addition with strides and offset"):
    val mat1 = Matrix.fromRows[Double](
      NArray[Double](1.0, 2.0, 3.0),
      NArray[Double](4.0, 5.0, 6.0),
      NArray[Double](7.0, 8.0, 9.0)
    )

    mat1 += 10.0

    assertMatrixEquals(
      mat1,
      Matrix.fromRows(
        NArray[Double](11.0, 12.0, 13.0),
        NArray[Double](14.0, 15.0, 16.0),
        NArray[Double](17.0, 18.0, 19.0)
      )
    )

    println(s"Matrix before addition: ${mat1.printMat}")
    println(s"subMatrix before addition: ${mat1.submatrix(0 to 1, 0 to 1).printMat}")
    mat1.submatrix(0 to 1, 0 to 1) += 1.0
    println(s"Matrix after addition: ${mat1.printMat}")

    assertMatrixEquals(
      mat1,
      Matrix.fromRows(
        NArray[Double](12.0, 13.0, 13.0),
        NArray[Double](15.0, 16.0, 16.0),
        NArray[Double](17.0, 18.0, 19.0)
      )
    )

    mat1.submatrix(1 to 2, 1 to 2) += 1.0
    assertMatrixEquals(
      mat1,
      Matrix.fromRows(
        NArray[Double](12.0, 13.0, 13.0),
        NArray[Double](15.0, 17.0, 17.0),
        NArray[Double](17.0, 19.0, 20.0)
      )
    )

  test("Submatrix addition can be zero copy"):
    val submat = mat36.submatrix(1 to 4, 1 to 4)

    // println(s"Submatrix before addition: ${submat.printMat}")
    submat += 10.0
    // println(s"Submatrix after addition: ${submat.printMat}")

    assertMatrixEquals(
      submat,
      Matrix.fromRows(
        NArray[Double](7.0, 8.0, 9.0, 10.0) + 10,
        NArray[Double](13.0, 14.0, 15.0, 16.0) + 10,
        NArray[Double](19.0, 20.0, 21.0, 22.0) + 10,
        NArray[Double](25.0, 26.0, 27.0, 28.0) + 10
      )
    )

    // tail loop one side
    val submat2 = mat36.submatrix(1 to 5, 0 to 5)
    // println(s"Submatrix2 before addition: ${submat2.printMat}")
    submat2 += 1.0
    // println(s"Submatrix2 after addition: ${submat2.printMat}")
    assertMatrixEquals(
      submat2,
      Matrix.fromRows(
        NArray[Double](7.0, 8.0, 9.0, 10.0, 11.0, 12.0),
        NArray[Double](13.0, 14.0, 15.0, 16.0, 17.0, 18.0),
        NArray[Double](19.0, 20.0, 21.0, 22.0, 23.0, 24.0),
        NArray[Double](25.0, 26.0, 27.0, 28.0, 29.0, 30.0),
        NArray[Double](31.0, 32.0, 33.0, 34.0, 35.0, 36.0)
      )
    )

    val submat3 = mat36.submatrix(0 to 5, 1 to 5)
    // println(s"Submatrix2 before addition: ${submat2.printMat}")
    submat3 += 1.0
    // println(s"Submatrix2 after addition: ${submat2.printMat}")
    assertMatrixEquals(
      submat3,
      Matrix.fromRows(
        NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0) + 1,
        NArray[Double](7.0, 8.0, 9.0, 10.0, 11.0) + 1,
        NArray[Double](13.0, 14.0, 15.0, 16.0, 17.0) + 1,
        NArray[Double](19.0, 20.0, 21.0, 22.0, 23.0) + 1,
        NArray[Double](25.0, 26.0, 27.0, 28.0, 29.0) + 1,
        NArray[Double](31.0, 32.0, 33.0, 34.0, 35.0) + 1
      )
    )
    val submat4 = mat36.submatrix(1 to 5, 1 to 5)
    // println(s"Submatrix2 before addition: ${submat2.printMat}")
    submat4 += 1.0
    // println(s"Submatrix2 after addition: ${submat2.printMat}")
    assertMatrixEquals(
      submat4,
      Matrix.fromRows(
        NArray[Double](7.0, 8.0, 9.0, 10.0, 11.0) + 1,
        NArray[Double](13.0, 14.0, 15.0, 16.0, 17.0) + 1,
        NArray[Double](19.0, 20.0, 21.0, 22.0, 23.0) + 1,
        NArray[Double](25.0, 26.0, 27.0, 28.0, 29.0) + 1,
        NArray[Double](31.0, 32.0, 33.0, 34.0, 35.0) + 1
      )
    )

end MatrixAdditionTest
