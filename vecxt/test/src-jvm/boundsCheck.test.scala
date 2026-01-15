package vecxt

import scala.util.chaining.*

import arrays.*
import BoundsCheck.DoBoundsCheck

class BoundsCheckSuite extends munit.FunSuite:

  lazy val v_fill = Array.tabulate(5)(i => i.toDouble)

  test("Bounds check") {
    intercept[VectorDimensionMismatch](v_fill.-(Array[Double](1, 2, 3))(using DoBoundsCheck.yes))
  }

  test("no bound check") {
    intercept[java.lang.IndexOutOfBoundsException](v_fill.-(Array[Double](1, 2, 3))(using DoBoundsCheck.no))
  }

end BoundsCheckSuite
