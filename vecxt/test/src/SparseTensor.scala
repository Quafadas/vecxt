package vecxt

import munit.FunSuite
import tensor.*
import scala.collection.mutable.ArrayBuffer

import narr.*
import vecxt.matrix.*
import vecxt.MatrixHelper.fromRows
import vecxt.BoundsCheck.DoBoundsCheck.yes

class SparseTensorSuite extends FunSuite:

  test("SparseTensor apply and access values") {
    val values = ArrayBuffer(1.0, 2.0, 3.0)
    val dims = Array(3, 3)
    val indices = ArrayBuffer(0, 4, 8)
    val tensor = SparseTensor(values, dims, indices)

    assertEquals(tensor.values, values)
    assertEquals(tensor.dims, dims)
    assertEquals(tensor.indices, indices)
  }

  test("SparseTensor apply method for accessing elements") {
    val values = ArrayBuffer(1.0, 2.0, 3.0)
    val dims = Array(3, 3)
    val indices = ArrayBuffer(0, 4, 8)
    val tensor = SparseTensor(values, dims, indices)

    assertEquals(tensor(Array(0, 0)), 1.0)
    assertEquals(tensor(Array(1, 1)), 2.0)
    assertEquals(tensor(Array(2, 2)), 3.0)
    assertEquals(tensor(Array(0, 1)), 0.0) // Non-existent element
  }

  test("SparseTensor with different dimensions") {
    val values = ArrayBuffer(1.0, 2.0)
    val dims = Array(2, 2, 2)
    val indices = ArrayBuffer(0, 7)
    val tensor = SparseTensor(values, dims, indices)

    assertEquals(tensor(Array(0, 0, 0)), 1.0, "tensor(Array(0, 0, 0)) should be 1.0")
    assertEquals(tensor(Array(1, 1, 1)), 2.0, "tensor(Array(1, 1, 1)) should be 2.0")
    assertEquals(tensor(Array(0, 1, 1)), 0.0, "tensor(Array(0, 1, 1)) should be 0.0") // Non-existent element
  }

  test("SparseTensor invalid input lengths") {
    val values = ArrayBuffer(1.0, 2.0)
    val dims = Array(2, 2)
    val indices = ArrayBuffer(0)

    intercept[IllegalArgumentException] {
      SparseTensor(values, dims, indices)
    }
  }

  test("SparseTensor add element") {
    val values = ArrayBuffer(1.0, 2.0, 3.0)
    val dims = Array(3, 3)
    val indices = ArrayBuffer(0, 4, 8)
    val tensor = SparseTensor(values, dims, indices)

    tensor.update(Array(1, 2), 4.0)
    assertEquals(tensor(Array(1, 2)), 4.0)
    assertEquals(tensor.values.last, 4.0)
    assertEquals(tensor.indices.last, 5)
  }

  test("SparseTensor to dense matrix multiplication") {
    val sparseValues = ArrayBuffer(1.0, 2.0, 3.0)
    val sparseDims = Array(3, 3)
    val sparseIndices = ArrayBuffer(0, 4, 8)
    val sparseTensor = SparseTensor(sparseValues, sparseDims, sparseIndices)

    val denseMatrix = Matrix.fromRows(
      NArray(
        NArray(1.0, 2.0, 3.0),
        NArray(4.0, 5.0, 6.0),
        NArray(7.0, 8.0, 9.0)
      )
    )

    val result = sparseTensor.multiply(denseMatrix)

    val expected = Matrix.fromRows(
      NArray(
        NArray[Double](1.0, 2.0, 3.0),
        NArray[Double](8.0, 10.0, 12.0),
        NArray[Double](21.0, 24.0, 27.0)
      )
    )

    assertVecEquals(result.raw, expected.raw)
  }

end SparseTensorSuite
