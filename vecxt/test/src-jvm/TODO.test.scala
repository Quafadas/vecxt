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

  // Was: "*= scalar throws for unsupported non-contiguous Float layouts", asserting a NotImplementedError for the
  // fixture below. That `???` is now a `foreach2D` fallback (needed so the scalar-left `d *= m` works for anything but
  // dense contiguous matrices), so the test asserts the values instead of the limitation.
  test("*= scalar scales every element of a non-contiguous Float layout, and nothing else"):
    // rows=2, cols=2, rowStride=2, colStride=5 over a length-8 array: neither stride is 1, so this is also the
    // `unitStrideAxis == -1` case. Elements live at raw(0), raw(2), raw(5), raw(7); the 90s are padding.
    val raw = Array[Float](1.0f, 90.0f, 2.0f, 91.0f, 92.0f, 3.0f, 93.0f, 4.0f)
    val mat = Matrix[Float](raw, 2, 2, 2, 5, 0)
    assert(!mat.hasSimpleContiguousMemoryLayout)

    mat *= 2.0f

    assertMatrixEquals(mat, Matrix.fromRows[Float](Array(2.0f, 6.0f), Array(4.0f, 8.0f)))
    assertVecEquals(raw, Array[Float](2.0f, 90.0f, 4.0f, 91.0f, 92.0f, 6.0f, 93.0f, 8.0f))

end TODO
