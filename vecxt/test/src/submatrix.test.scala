package vecxt
import munit.FunSuite
import narr.*
import all.*

class SubmatrixTest extends FunSuite:
  import vecxt.BoundsCheck.DoBoundsCheck.yes

  test("simple sub is zero copy"):
    val mat1 = Matrix.fromRows[Double](
      NArray[Double](1.0, 4.0, 2.0, 5.0, 3.0, 6.0),
      NArray[Double](1.0, 4.0, 2.0, 5.0, 3.0, 6.0) + 6.0,
      NArray[Double](1.0, 4.0, 2.0, 5.0, 3.0, 6.0) + 12.0
    )

    val submat = mat1.submatrix(1 to 2, 1 to 2) // 2x2 submatrix

    assertEquals(submat.numel, 4)
    assertEqualsDouble(submat(0, 0), 10.0, 0.0000001)
    assertEqualsDouble(submat(0, 1), 8.0, 0.0000001)
    assertEqualsDouble(submat(1, 0), 16.0, 0.0000001)
    assertEqualsDouble(submat(1, 1), 14.0, 0.0000001)

    // Check reference equality. This means our copy is indeed, zero-copy
    assert(submat.raw == mat1.raw)

  test("Non-contiguous submatrix selects correct data"):
    val mat1 = Matrix.fromRows[Double](
      NArray[Double](1.0, 4.0, 2.0, 5.0, 3.0, 6.0),
      NArray[Double](1.0, 4.0, 2.0, 5.0, 3.0, 6.0) + 6.0,
      NArray[Double](1.0, 4.0, 2.0, 5.0, 3.0, 6.0) + 12.0
    )

    val submat = mat1.submatrix(NArray[Int](0, 2), NArray[Int](0, 1, 5)) // 2x2 submatrix
    // println(submat.printMat)
    assertEquals(submat.numel, 6)
    assertEqualsDouble(submat(0, 0), 1.0, 0.0000001)
    assertEqualsDouble(submat(0, 1), 4.0, 0.0000001)
    assertEqualsDouble(submat(0, 2), 6.0, 0.0000001)
    assertEqualsDouble(submat(1, 0), 13.0, 0.0000001)
    assertEqualsDouble(submat(1, 1), 16.0, 0.0000001)
    assertEqualsDouble(submat(1, 2), 18.0, 0.0000001)

  test("Offset"):
    val mat1 = Matrix.fromRows[Double](
      NArray[Double](1.0, 2.0, 3.0),
      NArray[Double](1.0, 2.0, 3.0) + 3.0,
      NArray[Double](1.0, 2.0, 3.0) + 6.0
    )

    val submat1 = Matrix.fromColumns[Double](NArray(1.0, 2, 3))
    val submat2 = Matrix.fromColumns[Double](NArray(1.0, 2))

    for i <- 0.until(2) do
      val submat = mat1.submatrix(NArray(i, i + 1), ::)
      val result = submat @@ submat1 // zero copy
      val expected = submat.deepCopy @@ submat1.deepCopy
      assertMatrixEquals(result, expected)

      val resultT = submat.transpose @@ submat2
      val expectedT = submat.transpose.deepCopy @@ submat2.deepCopy
      assertMatrixEquals(result, expected)

      // val resultN_T = submat.transpose @@ submat1.transpose
      // val expectedN_T = submat.transpose.deepCopy @@ submat1.transpose.deepCopy
      // assertMatrixEquals(result, expected)
    end for

end SubmatrixTest
