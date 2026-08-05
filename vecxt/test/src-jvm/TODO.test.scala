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

    intercept[NotImplementedError] {
      left.`matmulInPlace!`(right, out, alpha = 1.0f, beta = 0.0f)
    }

  // Was "*:*= throws for unsupported non-matching Float and Boolean layouts": *:*='s non-fast-path branch is no
  // longer `???` (vecxt/src-jvm/floatmatrix.scala), so this now asserts the correct masked result instead of an
  // exception. `sub` has a nonzero offset (it's `base`'s columns 1..2), so it misses the dense/offset-0 fast path
  // and exercises the foreach2D fallback directly.
  test("*:*= applies the mask correctly for non-fast-path Float and Boolean layouts"):
    val base = Matrix.fromRows[Float](
      Array[Float](1.0f, 2.0f, 3.0f, 4.0f),
      Array[Float](5.0f, 6.0f, 7.0f, 8.0f),
      Array[Float](9.0f, 10.0f, 11.0f, 12.0f)
    )
    val sub = base(Range.Inclusive(0, 2, 1), Range.Inclusive(1, 2, 1))

    val mask = Matrix.fromRows[Boolean](
      Array[Boolean](false, true),
      Array[Boolean](true, false),
      Array[Boolean](false, true)
    )

    sub *:*= mask

    val want = Matrix.fromRows[Float](
      Array[Float](0.0f, 3.0f),
      Array[Float](6.0f, 0.0f),
      Array[Float](0.0f, 11.0f)
    )
    for
      i <- 0 until 3
      j <- 0 until 2
    do assertEquals(sub(i, j), want(i, j), s"*:*= at ($i, $j)")
    end for

  test("matrix-vector multiply throws for non-column-major Float matrices"):
    val rowMajor = Matrix[Float](
      Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f),
      2,
      3,
      3,
      1,
      0
    )

    intercept[NotImplementedError] {
      rowMajor.*(Array[Float](1.0f, 0.5f, -1.0f), alpha = 1.0f, beta = 0.0f)
    }

  test("*= scalar throws for unsupported non-contiguous Float layouts"):
    val raw = Array[Float](1.0f, 90.0f, 2.0f, 91.0f, 92.0f, 3.0f, 93.0f, 4.0f)
    val mat = Matrix[Float](raw, 2, 2, 2, 5, 0)

    intercept[NotImplementedError] {
      mat *= 2.0f
    }

end TODO
