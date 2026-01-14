package vecxt

import BoundsCheck.DoBoundsCheck.yes

import all.*

class SyntaxSuite extends munit.FunSuite:

  test("double matrix manipulation") {
    val d = 1.0
    val m = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0), (2, 2))

    d * m
    m * d

  }
end SyntaxSuite
