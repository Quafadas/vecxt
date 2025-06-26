package vecxt

import munit.FunSuite
import narr.*
import all.*
import BoundsCheck.DoBoundsCheck.yes
import dimensionExtender.DimensionExtender.Dimension.*
import MatrixInstance.update

class MatrixExtensionSuite extends FunSuite:

  // TODO will fail on JS, grrr.
  // test("print") {
  //   val mat1 = Matrix[Double](NArray(1.0, 4.0, 2.0, 5.0), (2, 2))
  //   assert(mat1.printMat.contains("4"))

  // }

  // test("transpose etc".only) {
  //   val mat1 = Matrix(NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (2, 3))
  //   val mat2 = Matrix(NArray(7.0, 9.0, 11.0, 8.0, 10, 12.0), (3, 2))
  //   val result2 = mat1 @@ mat2

  //   result2.printMat
  //   val result3 = Matrix.eye(2) + mat1 @@ mat2
  //   result3.printMat
  //   val mat3 = mat2.transpose + mat1
  //   println(mat2.transpose.printMat)
  //   mat3.raw.printArr
  //   mat3.printMat
  // }

  def mat1to9 = Matrix.fromRows[Double](
    NArray(1.0, 2.0, 3.0),
    NArray(4.0, 5.0, 6.0),
    NArray(7.0, 8.0, 9.0)
  )

  def raw1to9 = mat1to9.raw

  test("from rows") {
    assert(mat1to9.isDenseColMajor)

    assert(mat1to9.rows == 3)
    assert(mat1to9.cols == 3)
    assert(mat1to9.raw.length == 9)
    assert(mat1to9.colStride == 3)
    assert(mat1to9.rowStride == 1)
  }

  test("max reduction") {
    val mat1 = Matrix[Double](NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (3, 2))
    val maxR = mat1.max(Rows)
    assertMatrixEquals(
      maxR,
      Matrix[Double](
        NArray[Double](5.0, 4.0, 6.0),
        (3, 1)
      )
    )

    val maxC = mat1.max(Cols)
    assertMatrixEquals(maxC, Matrix[Double](NArray[Double](4.0, 6.0), (1, 2)))

  }

  test("Col major") {
    assert(mat1to9.isDenseColMajor)
    assert(mat1to9.hasSimpleContiguousMemoryLayout)
    assert(mat1to9.transpose.isDenseRowMajor)
    assert(mat1to9.transpose.hasSimpleContiguousMemoryLayout)
  }

  test("min reduction") {
    val mat1 = Matrix[Double](NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (3, 2))
    val minR = mat1.min(Rows)
    assertMatrixEquals(
      minR,
      Matrix[Double](
        NArray[Double](1.0, 3.0, 2.0),
        (3, 1)
      )
    )

    val minC = mat1.min(Cols)
    assertMatrixEquals(minC, Matrix[Double](NArray[Double](1.0, 3.0), (1, 2)))
  }

  test("product reduction") {
    val mat1 = Matrix[Double](NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (3, 2))
    val prodR = mat1.product(Rows)
    assertMatrixEquals(
      prodR,
      Matrix[Double](
        NArray[Double](5.0, 12.0, 12.0),
        (3, 1)
      )
    )

    val prodC = mat1.product(Cols)
    assertMatrixEquals(prodC, Matrix[Double](NArray[Double](8.0, 90.0), (1, 2)))
  }

  test("Some urnary ops") {
    val checkThis = mat1to9.exp
    mat1to9.log
    mat1to9.sqrt
    mat1to9.sin
    mat1to9.cos

    assertVecEquals[Double](checkThis.raw, raw1to9.exp)

  }

  test("element access") {

    val orig = Matrix.fromRows[Double](
      NArray(2.0, 0.0, -1.0),
      NArray(0.0, 3.0, 4.0)
    )

    assertEqualsDouble(orig((0, 0)), 2, 0.0001)
    assertEqualsDouble(orig((0, 1)), 0, 0.0001)
    assertEqualsDouble(orig((0, 2)), -1, 0.0001)
    assertEqualsDouble(orig((1, 0)), 0, 0.0001)
    assertEqualsDouble(orig((1, 1)), 3, 0.0001)
    assertEqualsDouble(orig((1, 2)), 4, 0.0001)

  }

  test("diagonal") {
    val orig = Matrix.fromRows[Double](
      NArray(2.0, 0.0, -1.0),
      NArray(1.0, 3.0, 4.0),
      NArray(0.0, 9.0, 5.0)
    )

    val diag = orig.diag
    assertVecEquals(diag, NArray[Double](2.0, 3.0, 5.0))

    val diag2 = orig.diag(1: Col, Vertical.Top, Horizontal.Right)
    assertVecEquals(diag2, NArray[Double](0.0, 4.0))

    val diag3 = orig.diag(1: Col, Vertical.Top, Horizontal.Left)

    assertVecEquals(diag3, NArray[Double](0.0, 1.0))

    val diag4 = orig.diag(1: Col, Vertical.Bottom, Horizontal.Left)
    assertVecEquals(diag4, NArray[Double](9.0, 1.0))

    val diag5 = orig.diag(1: Row, Horizontal.Left, Vertical.Top)
    assertVecEquals(diag5, NArray[Double](1.0, 0.0))

    val diag6 = orig.diag(orig.cols - 1, Vertical.Top, Horizontal.Left)
    assertVecEquals(diag6, NArray[Double](-1.0, 3.0, 0.0))

    val diag7 = orig.diag(orig.cols - 1, Vertical.Bottom, Horizontal.Left)
    assertVecEquals(diag7, NArray[Double](5.0, 3.0, 2.0))

    val diag8 = orig.diag(1: Row, Horizontal.Right, Vertical.Top)
    assertVecEquals(diag8, NArray[Double](4.0, 0.0))
  }

  test("diagonal irregular") {
    val orig = Matrix.fromRows[Double](
      NArray(2.0, 0.0),
      NArray(1.0, 3.0),
      NArray(0.0, 9.0)
    )

    val diag = orig.diag
    assertVecEquals(diag, NArray[Double](2.0, 3.0))

    val diag5 = orig.diag(1: Row, Horizontal.Left, Vertical.Top)
    assertVecEquals(diag5, NArray[Double](1.0, 0.0))
  }

  test("operator precedance") {
    val mat1 = Matrix.eye[Double](2)
    val mat2 = Matrix[Double](mat1.raw * 2, (mat1.rows, mat1.cols))

    val mat3 = mat1 +:+ mat1 @@ mat2
    assertEqualsDouble(mat3((0, 0)), 3.0, 0.00001)

  }

  test("dgemv precedance col major") {
    val mat1 = Matrix.fromRows(
      NArray(1.0, 2.0, 3.0),
      NArray(4.0, 5.0, 6.0)
    )

    val arr1 = NArray[Double](1.0, 2.0, 3.0)

    val arr2 = NArray[Double](1.0, 2.0)

    assertVecEquals(mat1 * arr1, NArray[Double](14.0, 32.0))

    // println("Mat1")
    // println(mat1.transpose.printMat)
    // println("Arr1")
    // println(arr2.printArr)

    // println("result")
    // println((mat1.transpose * arr2).printArr)
    // assertVecEquals(mat1.transpose * arr2, NArray[Double](6.0, 30.0))

  }

  test("zeros") {
    val tensor = Matrix.zeros[Double](2, 2)
    assertVecEquals[Double](tensor.raw, NArray[Double](0.0, 0.0, 0.0, 0.0))
  }

  test("eye") {
    val tensor = Matrix.eye[Double](2)
    assertVecEquals[Double](tensor.raw, NArray[Double](1.0, 0.0, 0.0, 1.0))
  }

  test("ones") {
    val tensor = Matrix.ones[Double]((2, 2))
    assertVecEquals[Double](tensor.raw, NArray[Double](1.0, 1.0, 1.0, 1.0))
  }

  test("multiply, divide, add, subtract elementwise") {
    val mat = Matrix.eye[Double](2)

    val t2 = mat + 1
    assertEqualsDouble(t2.sum, 6.0, 0.0001)

    val t3 = t2 * 2
    assertEqualsDouble(t3.sum, 12.0, 0.0001)

    val t4 = t3 - 5
    assertEqualsDouble(t4.sum, -8.0, 0.0001)

    val t5 = t4 / 8
    assertEqualsDouble(t5.sum, -1.0, 0.0001)

    assertVecEquals[Double](t5.raw, NArray[Double](-0.125, -0.375, -0.375, -0.125))

  }

  test("MAtrix addition and subtration") {
    val mat1 = Matrix.eye[Double](2)
    val mat2 = Matrix.eye[Double](2)
    val result = mat1 +:+ mat2
    assertVecEquals[Double](result.raw, NArray[Double](2.0, 0.0, 0.0, 2.0))

    val result2 = mat1 -:- mat2
    assertVecEquals[Double](result2.raw, NArray[Double](0.0, 0.0, 0.0, 0.0))
  }

  test("Matrix creation from nested NArray") {
    val nestedArr = NArray(
      NArray[Double](1.0, 2.0, 3.5), // col 1
      NArray[Double](3.0, 4.0, 5.0), // col 2
      NArray[Double](6.0, 7.0, 8.0) // col 2
    )
    val matrix = Matrix.fromColumns[Double](nestedArr.toArray*)
    assertVecEquals[Double](matrix.raw, NArray[Double](1.0, 2.0, 3.5, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0))

    val nestedArr3 = NArray(
      NArray[Double](1.0, 2.0, 3.5), // col 1
      NArray[Double](3.0, 4.0, 5.0) // col 2
    )
    val matrix3 = Matrix.fromColumns[Double](nestedArr3.toArray*)
    assertVecEquals[Double](matrix3.raw, NArray[Double](1.0, 2.0, 3.5, 3.0, 4.0, 5.0))

    val nestedArr2 = NArray(
      NArray[Double](1.0, 3.0), // row 1
      NArray[Double](2.0, 4.0), // row 2
      NArray[Double](3.5, 5.0) // row 3
    )
    val matrix2 = Matrix.fromRows[Double](nestedArr2.toArray*)
    assertVecEquals[Double](matrix2.raw, NArray[Double](1.0, 2.0, 3.5, 3.0, 4.0, 5.0))

  }

  test("Matrix multiplication") {
    val mat1 = Matrix.fromRows[Double](
      NArray(0.0, 0.0),
      NArray(1.0, 0.0)
    )
    val mat2 = Matrix.fromRows[Double](
      NArray(0.0, 1.0),
      NArray(0.0, 0.0)
    )
    val mult = Matrix[Double](NArray(0.0, 0.0, 0.0, 1.0), 2, 2)

    val result = mat1.matmul(mat2)
    assertMatrixEquals(result, mult)

  }

  test("Matrix multiplication2") {
    val mat1 = Matrix[Double](NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (2, 3))
    val mat2 = Matrix[Double](NArray(7.0, 9.0, 11.0, 8.0, 10, 12.0), (3, 2))
    val result = mat1.matmul(mat2)
    assertEquals(result.rows, 2)
    assertEquals(result.cols, 2)
    assertVecEquals[Double](result.raw, NArray(58.0, 139.0, 64.0, 154.0))
  }

  test("Matrix transpose") {
    val orig = Matrix.fromRows(
      NArray(2.0, 0.0, -1.0),
      NArray(0.0, 3.0, 4.0)
    )
    val transpose = Matrix.fromRows(
      NArray(2.0, 0.0),
      NArray(0.0, 3.0),
      NArray(-1.0, 4.0)
    )

    assertMatrixEquals(orig.transpose, transpose)

  }

  test("Tensor raw array retrieval") {
    val mat = Matrix[Double](NArray[Double](1.0, 2.0, 3.0, 4.0), (2, 2))
    assertVecEquals[Double](mat.raw, NArray(1.0, 2.0, 3.0, 4.0))

  }

  test("Tensor elementAt retrieval for 2D tensor") {
    val tensor = Matrix.fromRows(
      NArray[Double](1.0, 2.0),
      NArray[Double](3.0, 4.0)
    )
    assertEquals(tensor((0, 0)), 1.0)
    assertEquals(tensor((0, 1)), 2.0)
    assertEquals(tensor((1, 0)), 3.0)
    assertEquals(tensor((1, 1)), 4.0)
  }

  test("list indexes") {
    val indexes: NArray[RowCol] = NArray((0, 1), (1, 0), (1, 1))
    val newMat = mat1to9(indexes)

    assertEquals(newMat.raw.length, 9)

    assertVecEquals[Double](newMat.raw, NArray[Double](0.0, 4.0, 0.0, 2.0, 5.0, 0.0, 0.0, 0.0, 0.0))

  }

  // test("Tensor elementAt retrieval for 3D tensor") {
  //   val tensor = Matrix(NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), (2, 2, 2))
  //   assertEquals(tensor.elementAt((0, 0, 0)), 1.0)
  //   assertEquals(tensor.elementAt((0, 0, 1)), 2.0)
  //   assertEquals(tensor.elementAt((0, 1, 0)), 3.0)
  //   assertEquals(tensor.elementAt((0, 1, 1)), 4.0)
  //   assertEquals(tensor.elementAt((1, 0, 0)), 5.0)
  //   assertEquals(tensor.elementAt((1, 0, 1)), 6.0)
  //   assertEquals(tensor.elementAt((1, 1, 0)), 7.0)
  //   assertEquals(tensor.elementAt((1, 1, 1)), 8.0)
  // }

  // test("Tensor creation") {
  //   val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
  //   val tensor = Tensor(array, (2, 2))
  //   assertEquals(tensor.raw, array)
  // }

  // test("Vector creation") {
  //   val array = NArray[Double](1.0, 2.0, 3.0)
  //   val vector = Vector(array)
  //   assertEquals(vector.raw, array)
  // }

  test("Matrix creation") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Matrix[Double](array, (2, 2))
    assertEquals(matrix.raw, array)
  }

  test("Matrix rows and cols") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Matrix[Double](array, (2, 2))
    assertEquals(matrix.rows, 2)
    assertEquals(matrix.cols, 2)
  }

  test("Matrix row extraction") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Matrix[Double](array, (2, 2))
    val row = matrix.row(0)
    assertVecEquals(row, NArray[Double](1.0, 3.0))

    val row2 = matrix.row(1)
    assertVecEquals(row2, NArray[Double](2.0, 4.0))
  }

  test("update") {
    val mat = Matrix[Double](NArray(1.0, 2.0, 3.0, 4.0), (2, 2))
    mat((0, 0)) = 5.0
    assertEquals(mat.raw(0), 5.0)

    mat((1, 1)) = 6.0
    assertEquals(mat.raw(3), 6.0)

    mat((1, 0)) = 7.0
    assertEquals(mat.raw(1), 7.0)
  }

  test("trace") {
    val mat = Matrix[Double](NArray(1.0, 2.0, 3.0, 4.0), (2, 2))
    assertEquals(mat.trace, 5.0)
  }

  test("Matrix column extraction") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Matrix[Double](array, (2, 2))
    val col1 = matrix.col(0)
    assertVecEquals(col1, NArray[Double](1.0, 2.0))

    val col = matrix.col(1)
    assertVecEquals(col, NArray[Double](3.0, 4.0))

  }

  test("That we have to provide ints for dimensions") {
    // We should allow non-integer values in the dimensions of the Tuple.
    val code = "Matrix((1, 1.0), NArray.ofSize[Double](6))"
    val code2 = """Matrix((1, "s"), NArray.ofSize[Double](6))"""
    compileErrors(code)
    compileErrors(code2)
  }

  test("matrix scale") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Matrix[Double](array, (2, 2))
    matrix *= 2
    assertVecEquals[Double](matrix.raw, NArray[Double](2.0, 4.0, 6.0, 8.0))
  }

  test("matrix ops") {
    val mat1 = Matrix.eye[Double](3) // multiplication in place
    mat1 *= 2
    val mat2 = Matrix.eye[Double](3) +:+ Matrix.eye[Double](3) // addition
    assertVecEquals[Double](mat1.raw, mat2.raw)
  }

  test("matrix * elementwise") {
    val mat1 = Matrix.eye[Double](3) // multiplication in place
    mat1 *= 2
    val mat2 = Matrix.eye[Double](3) +:+ Matrix.eye[Double](3) // addition
    assertVecEquals[Double](mat1.raw, mat2.raw)

    val bah = mat1.hadamard(mat2)

    assertVecEquals[Double](mat1.raw, mat2.raw)

  }

  test("invalid matrix fails to build") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    intercept[InvalidMatrix](
      Matrix[Double](array, (2, 3))
    )

    intercept[InvalidMatrix](
      Matrix[Double]((2, 3), array)
    )

    intercept[MatrixDimensionMismatch](
      Matrix.eye[Double](1) +:+ Matrix.eye[Double](2)
    )

    intercept[java.lang.AssertionError](
      Matrix.fromColumns(
        NArray[Double](3.0, 2.0, 3.0),
        NArray[Double](2.0, 1.0)
      )
    )

    intercept[java.lang.AssertionError](
      Matrix.fromRows(
        NArray[Double](3.0, 2.0, 3.0),
        NArray[Double](2.0, 1.0)
      )
    )
  }

  test("update") {
    val mat = Matrix.fromRows[Double](
      NArray[Double](1.0, 2.0, 3.0),
      NArray[Double](4.0, 5.0, 6.0)
    )
    mat.update(1, 0, 10.0)

    assertEquals(mat(1, 0), 10.0)
    val m2 = mat.transpose
    m2(0, 1) = (20.0)
    assertEquals(m2(0, 1), 20.0)

    val mat2 = mat1to9
    mat2.update(1, 0, 10.0)
    assertEquals(mat2(1, 0), 10.0)
    val m3 = mat2.transpose
    m3(0, 1) = (20.0)
    assertEquals(m3(0, 1), 20.0)
  }

  test("deep copy") {
    val mat = Matrix.fromRows[Double](
      NArray[Double](1.0, 2.0, 3.0),
      NArray[Double](4.0, 5.0, 6.0)
    )
    assert(mat.isDenseColMajor)
    assertEquals(mat.rows, 2)
    assertEquals(mat.cols, 3)
    assertEquals(mat.rowStride, 1)
    assertEquals(mat.colStride, 2)
    val copy = mat.deepCopy
    assertEquals(copy.rowStride, 1)
    assertEquals(copy.colStride, 2)
    assertEquals(copy.rows, 2)
    assertEquals(copy.cols, 3)
    assertEquals(copy.offset, 0)
    assertMatrixEquals(mat, copy)
    assert(mat.raw ne copy.raw)
  }

  test("row syntax returns a hard copied array") {
    val mat = mat1to9
    assertVecEquals(mat.row(0), NArray[Double](1.0, 2.0, 3.0))
    assertVecEquals(mat.row(1), NArray[Double](4.0, 5.0, 6.0))
    assertVecEquals(mat.row(2), NArray[Double](7.0, 8.0, 9.0))
  }

  test("slice syntax (zero copy matrix)") {
    val mat = mat1to9
    val a = mat(::, ::)
    assertVecEquals[Double](a.raw, mat.raw)

    val b = mat(NArray[Int](1), ::)
    assertMatrixEquals(Matrix(NArray[Double](4.0, 5.0, 6.0), 1, 3), b)

    val b1 = mat(NArray[Int](1), ::) // this should be the same.
    assertMatrixEquals(Matrix(NArray[Double](4.0, 5.0, 6.0), 1, 3), b1)

    // A slice of a slice...
    val b2 = b1(::, NArray(2)) // this should be the same.
    assertMatrixEquals(Matrix(NArray[Double](6.0), 1, 1), b2)

    val c = mat(::, NArray[Int](1))
    assertMatrixEquals(Matrix(NArray[Double](2.0, 5.0, 8.0), 3, 1), c)

    val d = mat(NArray[Int](1), NArray[Int](1))
    assertMatrixEquals(Matrix(NArray[Double](5.0), 1, 1), d)

    val e = mat(0 to 1, 0 to 1)
    assertMatrixEquals(Matrix(NArray[Double](1.0, 4.0, 2.0, 5.0), 2, 2), e)

    val f = mat(NArray.from[Int](Array(0, 2)), 0 to 1)
    assertMatrixEquals(Matrix(NArray[Double](1.0, 7.0, 2.0, 8.0), 2, 2), f)

  }

  test("hadamard product") {
    val mat1 = Matrix.eye[Double](3)
    val mat2 = Matrix.eye[Double](3) * 2
    val result = mat1.hadamard(mat2)
    assertVecEquals[Double](result.raw, NArray[Double](2.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 2.0))
  }

  test("map rows") {
    val mapped = mat1to9.mapRows[Double](row => row * 2)
    assertVecEquals[Double](mapped.raw, mat1to9.raw * 2)

    val mapped2 = mat1to9.mapRowsToScalar[Double](row => row.sum)
    assertVecEquals[Double](mapped2.raw, NArray[Double](6.0, 15.0, 24.0))

    val mapped3 = mat1to9.mapRows[Double](row => row / row.sum)
    assert(mapped3.rows == 3)
    assert(mapped3.cols == 3)

    assertVecEquals[Double](mapped3.row(0), NArray[Double](1.0, 2.0, 3.0) / 6.0)
  }

  test("map rows in place") {
    val mat = mat1to9
    mat.mapRowsInPlace(row => row / row.sum)
    assert(mat.rows == 3)
    assert(mat.cols == 3)
    assertVecEquals[Double](mat.row(0), NArray[Double](1.0, 2.0, 3.0) / 6.0)
    assertVecEquals[Double](mat.row(1), NArray[Double](4.0, 5.0, 6.0) / 15.0)
    assertVecEquals[Double](mat.row(2), NArray[Double](7.0, 8.0, 9.0) / 24.0)
  }

  test("map cols in place") {
    val mat = mat1to9
    mat.mapColsInPlace(col => col / col.sum)
    assert(mat.rows == 3)
    assert(mat.cols == 3)
    assertVecEquals[Double](mat.col(0), NArray[Double](1.0, 4.0, 7.0) / 12.0)
    assertVecEquals[Double](mat.col(1), NArray[Double](2.0, 5.0, 8.0) / 15.0)
    assertVecEquals[Double](mat.col(2), NArray[Double](3.0, 6.0, 9.0) / 18.0)
  }

  test("map cols") {
    val mapped = mat1to9.mapCols[Double](col => col * 2.0)
    assertVecEquals[Double](mapped.raw, mat1to9.raw * 2)

    val mapped2 = mat1to9.mapColsToScalar[Double](col => col.sum)
    assertVecEquals[Double](mapped2.raw, NArray[Double](12.0, 15.0, 18.0))

    val mapped3 = mat1to9.mapCols[Double](col => col / col.sum)
    assertVecEquals[Double](mapped3.col(0), NArray[Double](1.0, 4.0, 7.0) / 12.0)
  }

  test("map cols non square") {

    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(4.0, 5.0, 6.0)
    )
    val mapped = mat.mapCols[Double](col => col - col.max)

    assertVecEquals[Double](mapped.row(0), NArray[Double](-3.0, -3.0, -3.0))
    assertVecEquals[Double](mapped.row(1), NArray[Double](-0.0, -0.0, 0.0))

    val mapped2 = mat.mapColsToScalar[Double](col => col.sum)
    assertVecEquals[Double](mapped2.raw, NArray[Double](5.0, 7.0, 9.0))
  }

  test("map rows non square") {

    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(4.0, 5.0, 6.0)
    )
    val mapped = mat.mapRows[Double](row => row - row.max)

    assertVecEquals[Double](mapped.row(0), NArray[Double](-2.0, -1.0, 0.0))
    assertVecEquals[Double](mapped.row(1), NArray[Double](-2.0, -1.0, 0.0))

    val mapped2 = mat.mapRowsToScalar[Double](col => col.sum)
    assertVecEquals[Double](mapped2.raw, NArray[Double](6.0, 15.0))

  }

  test("update row") {
    val mat: Matrix[Double] = mat1to9
    mat.updateInPlace(NArray[Int](0), ::, NArray[Double](10.0, 20.0, 30.0))
    // mat(NArray[Int](2), ::) = NArray[Double](10.0, 20.0, 30.0)
    // assertVecEquals(mat.row(0), NArray[Double](10.0, 20.0, 30.0))
    // assertVecEquals(mat.row(2), NArray[Double](10.0, 20.0, 30.0))
  }

  test("map rows non square") {
    val mat1 = Matrix[Double](
      NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0),
      (2, 3)
    )
    val mapped = mat1.mapRows[Double](r => r / r.sum)
    assertVecEquals[Double](mapped.row(0), NArray[Double](1.0, 2.0, 3.0) / 6.0)
  }

  test("horzcat") {
    val mat1 = Matrix.fromRows[Double](
      NArray[Double](1.0, 2.0, 3.0),
      NArray[Double](4.0, 5.0, 6.0)
    )
    val mat2 = Matrix.fromRows[Double](
      NArray[Double](7.0, 8.0, 9.0),
      NArray[Double](10.0, 11.0, 12.0)
    )
    val expected = Matrix.fromRows[Double](
      NArray[Double](1.0, 2.0, 3.0, 7.0, 8.0, 9.0),
      NArray[Double](4.0, 5.0, 6.0, 10.0, 11.0, 12.0)
    )
    val result = mat1.horzcat(mat2)
    assertMatrixEquals(result, expected)
  }

  test("vertcat") {
    val mat1 = Matrix.fromRows(
      NArray[Double](1.0, 2.0, 3.0),
      NArray[Double](4.0, 5.0, 6.0)
    )
    val mat2 = Matrix.fromRows(
      NArray[Double](7.0, 8.0, 9.0),
      NArray[Double](10.0, 11.0, 12.0)
    )
    val expected = Matrix.fromRows(
      NArray[Double](1.0, 2.0, 3.0),
      NArray[Double](4.0, 5.0, 6.0),
      NArray[Double](7.0, 8.0, 9.0),
      NArray[Double](10.0, 11.0, 12.0)
    )
    val result = mat1.vertcat(mat2)
    assertMatrixEquals(result, expected)
  }

  test("selection with two ranges") {
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0, 10.0),
      NArray(4.0, 5.0, 6.0, 10.0),
      NArray(7.0, 8.0, 9.0, 10.0),
      NArray(7.0, 8.0, 9.0, 10.0),
      NArray(7.0, 8.0, 9.0, 10.0)
    )

    val expected = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(4.0, 5.0, 6.0),
      NArray(7.0, 8.0, 9.0)
    )
    assertMatrixEquals(mat(0 to 2, 0 to 2), expected)
  }

end MatrixExtensionSuite
