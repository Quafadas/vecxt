package vecxt

import scala.util.chaining.*

import doublearrays.*

class BoundsCheckSuite extends munit.FunSuite:

  lazy val v_fill = Array.tabulate(5)(i => i.toDouble)

  test("Bounds check") {
    intercept[vecxt.VectorDimensionMismatch](v_fill.-(Array[Double](1, 2, 3)))
  }

end BoundsCheckSuite
