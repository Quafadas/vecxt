package vecxt

import munit.FunSuite

import all.*

class TODO extends FunSuite:

  test("matmulInPlace! throws for unsupported general Float layouts"):
    val left = Matrix[Float](
      Array[Float](1.0f, 90.0f, 2.0f, 91.0f, 92.0f, 3.0f, 93.0f, 4.0f),
      2,
      2,
      2,
      5,
      0
    )
    val right = Matrix[Float](
      Array[Float](5.0f, 80.0f, 6.0f, 81.0f, 82.0f, 7.0f, 83.0f, 8.0f),
      2,
      2,
      2,
      5,
      0
    )
    val out = Matrix.zeros[Float]((2, 2))

    intercept[UnsupportedLayoutException] {
      left.`matmulInPlace!`(right, out, alpha = 1.0f, beta = 0.0f)
    }


end TODO
