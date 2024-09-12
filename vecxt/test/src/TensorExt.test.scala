package vecxt

import munit.FunSuite
import narr.*
import vecxt.Tensors.*
import vecxt.extensions.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

class TensorExtensionSuite extends FunSuite:

  test("Matrix multiplication") {
    val mat1 = Matrix(NArray(0.0, 0.0, 1.0, 0.0), (2, 2))
    val mat2 = Matrix(NArray(0.0, 1.0, 0.0, 0.0), (2, 2))
    val result = mat1.matmul(mat2)
    assertVecEquals(result.raw, NArray(1.0, 0.0, 0.0, 0.0))
  }

  test("Matrix multiplication2") {
    val mat1 = Matrix(NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (2, 3))
    val mat2 = Matrix(NArray(7.0, 9.0, 11.0, 8.0, 10, 12.0), (3, 2))
    val result = mat1.matmul(mat2)
    assertEquals(result.rows, 2)
    assertEquals(result.cols, 2)
    assertVecEquals(result.raw, NArray(58.0, 139.0, 64.0, 154.0))
  }

  test("Matrix transpose") {
    val originalArray = NArray[Double](1, 2, 3, 4, 5, 6)
    val matrix = Matrix(originalArray, (2, 3))

    println(matrix.print)

    val transposedMatrix = matrix.transpose

    val expectedArray = NArray[Double](1, 4, 2, 5, 3, 6)
    val expectedMatrix = Matrix(expectedArray, (3, 2))

    assertEquals(transposedMatrix.raw.toList, expectedMatrix.raw.toList)
    assertEquals(transposedMatrix.rows, expectedMatrix.rows)
    assertEquals(transposedMatrix.cols, expectedMatrix.cols)
  }

  test("Tensor raw array retrieval") {
    val vec = Vector(NArray[Double](1.0, 2.0, 3.0))
    assertVecEquals(vec.raw, NArray(1.0, 2.0, 3.0))

    val mat = Matrix(NArray[Double](1.0, 2.0, 3.0, 4.0), (2, 2))
    assertVecEquals(mat.raw, NArray(1.0, 2.0, 3.0, 4.0))

  }

  test("Tensor elementAt retrieval for 1D tensor") {
    val tensor = Vector(NArray[Double](1.0, 2.0, 3.0))
    assertEquals(tensor.elementAt(Tuple1(0)), 1.0)
    assertEquals(tensor.elementAt(Tuple1(1)), 2.0)
    assertEquals(tensor.elementAt(Tuple1(2)), 3.0)
  }

  test("Tensor elementAt retrieval for 2D tensor") {
    val tensor = Matrix(NArray[Double](1.0, 2.0, 3.0, 4.0), (2, 2))
    assertEquals(tensor.elementAt((0, 0)), 1.0)
    assertEquals(tensor.elementAt((0, 1)), 2.0)
    assertEquals(tensor.elementAt((1, 0)), 3.0)
    assertEquals(tensor.elementAt((1, 1)), 4.0)
  }

  test("Tensor elementAt retrieval for 3D tensor") {
    val tensor = Tensors.Tensor(NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), (2, 2, 2))
    assertEquals(tensor.elementAt((0, 0, 0)), 1.0)
    assertEquals(tensor.elementAt((0, 0, 1)), 2.0)
    assertEquals(tensor.elementAt((0, 1, 0)), 3.0)
    assertEquals(tensor.elementAt((0, 1, 1)), 4.0)
    assertEquals(tensor.elementAt((1, 0, 0)), 5.0)
    assertEquals(tensor.elementAt((1, 0, 1)), 6.0)
    assertEquals(tensor.elementAt((1, 1, 0)), 7.0)
    assertEquals(tensor.elementAt((1, 1, 1)), 8.0)
  }

  test("Tensor creation") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val tensor = Tensors.Tensor(array, (2, 2))
    assertEquals(tensor.raw, array)
  }

  test("Vector creation") {
    val array = NArray[Double](1.0, 2.0, 3.0)
    val vector = Tensors.Vector(array)
    assertEquals(vector.raw, array)
  }

  test("Matrix creation") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Tensors.Matrix(array, (2, 2))
    assertEquals(matrix.raw, array)
  }

  test("Matrix rows and cols") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Tensors.Matrix(array, (2, 2))
    assertEquals(matrix.rows, 2)
    assertEquals(matrix.cols, 2)
  }

  test("Matrix row extraction") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Tensors.Matrix(array, (2, 2))
    val row = matrix.row(1)
    assertVecEquals(row, NArray[Double](3.0, 4.0))

    val row2 = matrix.row(0)
    assertVecEquals(row2, NArray[Double](1.0, 2.0))
  }

  test("Matrix column extraction") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Tensors.Matrix(array, (2, 2))
    val col = matrix.col(1)
    assertVecEquals(col, NArray[Double](2.0, 4.0))
  }
end TensorExtensionSuite
