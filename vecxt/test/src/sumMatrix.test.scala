package vecxt
import munit.FunSuite
import narr.*
import vecxt.BoundsCheck.BoundsCheck
import all.*
import dimensionExtender.DimensionExtender.Dimension.*

class SumMatrixTest extends FunSuite:
  import vecxt.BoundsCheck.DoBoundsCheck.yes

  def mat1to9 = Matrix.fromRows[Double](
    NArray[Double](1.0, 2.0, 3.0),
    NArray[Double](4.0, 5.0, 6.0),
    NArray[Double](7.0, 8.0, 9.0)
  )

  test("simple sum"):
    val mat1 = Matrix[Double](NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), 3, 2)
    assertEqualsDouble(mat1.sum, 21.0, 0.000000001)

  test("sum reduction"):
    val mat1 = Matrix[Double](NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (3, 2))
    val sumR = mat1.sum(Rows)
    assertMatrixEquals(
      sumR,
      Matrix[Double](
        NArray[Double](6.0, 7.0, 8.0),
        (3, 1)
      )
    )

    val sumC = mat1.sum(Cols)
    assertMatrixEquals(sumC, Matrix[Double](NArray[Double](7.0, 14.0), (1, 2)))

  test("sum different strides"):
    val mat1 = Matrix[Double](NArray(1.0, 2.0, 3.0, 4.0), 2, 2, 1, 2)
    val mat2 = Matrix[Double](NArray(5.0, 1.0, 7.0, 8.0), 2, 2, 2, 1)
    val matSummed = Matrix[Double](NArray(6.0, 9.0, 4.0, 12.0), 2, 2, 1, 2)

    assertMatrixEquals(mat1 + mat2, matSummed)

end SumMatrixTest
