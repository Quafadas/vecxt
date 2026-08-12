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

  // Removed: "matrix-vector multiply throws for non-column-major Float matrices". It asserted a NotImplementedError
  // for a dense row-major operand, which is precisely the `???` this branch replaced — `*` now picks TRANS and lda
  // from the strides and handles that layout, so the test was pinning the limitation rather than any behaviour worth
  // keeping. The positive case is covered in the shared FloatMatVecSuite, which runs the same 2x3 row-major shape
  // (among five layouts) and checks the values rather than just that something happens.

  test("*= scalar throws for unsupported non-contiguous Float layouts"):
    val raw = Array[Float](1.0f, 90.0f, 2.0f, 91.0f, 92.0f, 3.0f, 93.0f, 4.0f)
    val mat = Matrix[Float](raw, 2, 2, 2, 5, 0)

    intercept[NotImplementedError] {
      mat *= 2.0f
    }

end TODO
