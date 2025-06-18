package vecxt
import munit.FunSuite
import narr.*
import vecxt.BoundsCheck.BoundsCheck
import all.*
import dimensionExtender.DimensionExtender.Dimension.*

class SumMatrixTest extends FunSuite:
  import vecxt.BoundsCheck.DoBoundsCheck.yes

  test("simple sum"):
    val mat1 = Matrix[Double](NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (3, 2))
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
end SumMatrixTest
