package vecxtensions

import vecxt.all.*
import spire.math.Complex
import munit.FunSuite
import vecxt.BoundsCheck
import spire.implicits.*


import vecxtensions.SpireExt.*

class MatrixExtensionSuite extends FunSuite:

  import BoundsCheck.DoBoundsCheck.yes

  def assertVecEquals[A](v1: Array[A], v2: Array[A])(implicit loc: munit.Location): Unit =
    var i: Int = 0;
    while i < v1.length do
      munit.Assertions.assertEquals(v1(i), v2(i), clue = s"at index $i")
      i += 1
    end while
  end assertVecEquals

  test("Higher kinded matmul") {
    val mat1 = Matrix.fromRows(
      NArray(1L, 2L, 3L),
      NArray(4L, 5L, 6L)
    )

    val mat2 = Matrix.fromRows(
      NArray(7L, 8L),
      NArray(9L, 10L),
      NArray(11L, 12L)
    )

    val result = Matrix.fromRows(
      NArray(58L, 64L),
      NArray(139L, 154L)
    )

    val mult = mat1 @@@ mat2
    assertVecEquals(mult.raw, result.raw)

  }

  test("Spire matmul") {

    val mat1 = Matrix.fromRows[Complex[Double]](
      Array[Complex[Double]](Complex(1.0, -1.0), Complex(0.0, 2.0), Complex(-2.0, 1.0)),
      Array[Complex[Double]](Complex(0.0, -3.0), Complex(3.0, -2.0), Complex(-1.0, -1.0))
    )

    val mat2 = Matrix.fromRows[Complex[Double]](
      Array[Complex[Double]](Complex(0.0, -2.0), Complex(1.0, -4.0)),
      Array[Complex[Double]](Complex(-1.0, 3.0), Complex(2.0, -3.0)),
      Array[Complex[Double]](Complex(-2.0, 1.0), Complex(-4.0, 1.0))
    )

    val result = Matrix.fromRows[Complex[Double]](
      Array[Complex[Double]](Complex(-5.0, -8.0), Complex(10.0, -7.0)),
      Array[Complex[Double]](Complex(0.0, 12.0), Complex(-7.0, -13.0))
    )

    val mult: Matrix[Complex[Double]] = mat1 @@@ mat2
    assertVecEquals(mult.raw, result.raw)
  }

end MatrixExtensionSuite
