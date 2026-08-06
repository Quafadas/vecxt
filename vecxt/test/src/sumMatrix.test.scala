package vecxt
import munit.FunSuite

import all.*
import dimensionExtender.DimensionExtender.Dimension.*

class SumMatrixTest extends FunSuite:

  def mat1to9 = Matrix.fromRows[Double](
    Array[Double](1.0, 2.0, 3.0),
    Array[Double](4.0, 5.0, 6.0),
    Array[Double](7.0, 8.0, 9.0)
  )

  test("simple sum"):
    val mat1 = Matrix[Double](Array(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), 3, 2)
    assertEqualsDouble(mat1.sum, 21.0, 0.000000001)

  test("sum reduction"):
    val mat1 = Matrix[Double](Array(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (3, 2))
    val sumR = mat1.sum(Rows)
    assertMatrixEquals(
      sumR,
      Matrix[Double](
        Array[Double](6.0, 7.0, 8.0),
        (3, 1)
      )
    )

    val sumC = mat1.sum(Cols)
    assertMatrixEquals(sumC, Matrix[Double](Array[Double](7.0, 14.0), (1, 2)))

  test("sum reduction, row-major"):
    // Same logical matrix as "sum reduction" above ([[1,5],[4,3],[2,6]]), but dense row-major instead of
    // column-major, so reduceAlongDimension's internal indexing (which used to hardcode the column-major formula)
    // is exercised on the layout it used to get wrong.
    val mat1 = Matrix[Double](Array(1.0, 5.0, 4.0, 3.0, 2.0, 6.0), 3, 2, 2, 1, 0)
    assert(mat1.isDenseRowMajor)

    val sumR = mat1.sum(Rows)
    assertMatrixEquals(
      sumR,
      Matrix[Double](
        Array[Double](6.0, 7.0, 8.0),
        (3, 1)
      )
    )

    val sumC = mat1.sum(Cols)
    assertMatrixEquals(sumC, Matrix[Double](Array[Double](7.0, 14.0), (1, 2)))

  test("sum different strides"):
    val mat1 = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0), 2, 2, 1, 2)
    val mat2 = Matrix[Double](Array(5.0, 1.0, 7.0, 8.0), 2, 2, 2, 1)
    val matSummed = Matrix[Double](Array(6.0, 9.0, 4.0, 12.0), 2, 2, 1, 2)

    assertMatrixEquals(mat1 + mat2, matSummed)

end SumMatrixTest
