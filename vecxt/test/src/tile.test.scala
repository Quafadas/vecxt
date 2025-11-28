package vecxt

import narr.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import all.*

class MatrixHelperTileSuite extends munit.FunSuite:

  test("tile 2x2 matrix by 2x2"):
    val m = Matrix.fromRows[Int](
      NArray[Int](1, 2),
      NArray[Int](3, 4)
    )

    val tiled = Matrix.tile(m, 2, 2)

    // Expected: [[1,2,1,2], [3,4,3,4], [1,2,1,2], [3,4,3,4]]
    assertEquals(tiled.rows, 4)
    assertEquals(tiled.cols, 4)

    assertMatrixEquals(
      tiled,
      Matrix.fromRows(
        NArray[Int](1, 2, 1, 2),
        NArray[Int](3, 4, 3, 4),
        NArray[Int](1, 2, 1, 2),
        NArray[Int](3, 4, 3, 4)
      )
    )

  test("tile 1x3 row vector by 3x2"):
    val m = Matrix.fromRows[Double](
      NArray[Double](1.0, 2.0, 3.0)
    )

    val tiled = Matrix.tile(m, 3, 2)

    assertEquals(tiled.rows, 3)
    assertEquals(tiled.cols, 6)

    assertMatrixEquals(
      tiled,
      Matrix.fromRows(
        NArray[Double](1.0, 2.0, 3.0, 1.0, 2.0, 3.0),
        NArray[Double](1.0, 2.0, 3.0, 1.0, 2.0, 3.0),
        NArray[Double](1.0, 2.0, 3.0, 1.0, 2.0, 3.0)
      )
    )

  test("tile 3x1 column vector by 2x3"):
    val m = Matrix.fromRows[Int](
      NArray[Int](1),
      NArray[Int](2),
      NArray[Int](3)
    )

    val tiled = Matrix.tile(m, 2, 3)

    assertEquals(tiled.rows, 6)
    assertEquals(tiled.cols, 3)

    assertMatrixEquals(
      tiled,
      Matrix.fromRows(
        NArray[Int](1, 1, 1),
        NArray[Int](2, 2, 2),
        NArray[Int](3, 3, 3),
        NArray[Int](1, 1, 1),
        NArray[Int](2, 2, 2),
        NArray[Int](3, 3, 3)
      )
    )

  test("tile by 1x1 returns copy"):
    val m = Matrix.fromRows[Double](
      NArray[Double](1.0, 2.0),
      NArray[Double](3.0, 4.0),
      NArray[Double](5.0, 6.0)
    )

    val tiled = Matrix.tile(m, 1, 1)

    assertEquals(tiled.rows, m.rows)
    assertEquals(tiled.cols, m.cols)
    assertMatrixEquals(tiled, m)

  test("tile 1x1 matrix by 3x3"):
    val m = Matrix.fromRows[Int](
      NArray[Int](42)
    )

    val tiled = Matrix.tile(m, 3, 3)

    assertEquals(tiled.rows, 3)
    assertEquals(tiled.cols, 3)

    assertMatrixEquals(
      tiled,
      Matrix.fromRows(
        NArray[Int](42, 42, 42),
        NArray[Int](42, 42, 42),
        NArray[Int](42, 42, 42)
      )
    )

  test("tile non-square matrix 3x2 by 2x3"):
    val m = Matrix.fromRows[Double](
      NArray[Double](1.0, 2.0),
      NArray[Double](3.0, 4.0),
      NArray[Double](5.0, 6.0)
    )

    val tiled = Matrix.tile(m, 2, 3)

    assertEquals(tiled.rows, 6)
    assertEquals(tiled.cols, 6)

    assertMatrixEquals(
      tiled,
      Matrix.fromRows(
        NArray[Double](1.0, 2.0, 1.0, 2.0, 1.0, 2.0),
        NArray[Double](3.0, 4.0, 3.0, 4.0, 3.0, 4.0),
        NArray[Double](5.0, 6.0, 5.0, 6.0, 5.0, 6.0),
        NArray[Double](1.0, 2.0, 1.0, 2.0, 1.0, 2.0),
        NArray[Double](3.0, 4.0, 3.0, 4.0, 3.0, 4.0),
        NArray[Double](5.0, 6.0, 5.0, 6.0, 5.0, 6.0)
      )
    )

  test("tile with large repetition factor"):
    val m = Matrix.fromRows[Int](
      NArray[Int](1, 2),
      NArray[Int](3, 4)
    )

    val tiled = Matrix.tile(m, 5, 5)

    assertEquals(tiled.rows, 10)
    assertEquals(tiled.cols, 10)
    assertEquals(tiled.numel, 100)

    // Spot check corners and middle
    // Original matrix: [[1, 2], [3, 4]]
    // Position (0,0) -> tile (0,0), local (0,0) = 1
    assertEquals(tiled(0, 0), 1)
    // Position (0,9) -> tile (0,4), local (0,1) = 2
    assertEquals(tiled(0, 9), 2)
    // Position (9,0) -> tile (4,0), local (1,0) = 3
    assertEquals(tiled(9, 0), 3)
    // Position (9,9) -> tile (4,4), local (1,1) = 4
    assertEquals(tiled(9, 9), 4)
    // Position (5,5) -> tile (2,2), local (1,1) = 4
    assertEquals(tiled(5, 5), 4)
    // Position (4,4) -> tile (2,2), local (0,0) = 1
    assertEquals(tiled(4, 4), 1)

  test("tile preserves element values exactly"):
    val m = Matrix.fromRows[Double](
      NArray[Double](1.1, 2.2, 3.3),
      NArray[Double](4.4, 5.5, 6.6)
    )

    val tiled = Matrix.tile(m, 2, 2)

    // Check all tiles contain exact values
    for r <- 0 until 2; c <- 0 until 2 do
      val offsetR = r * 2
      val offsetC = c * 3
      assertEquals(tiled(offsetR + 0, offsetC + 0), 1.1)
      assertEquals(tiled(offsetR + 0, offsetC + 1), 2.2)
      assertEquals(tiled(offsetR + 0, offsetC + 2), 3.3)
      assertEquals(tiled(offsetR + 1, offsetC + 0), 4.4)
      assertEquals(tiled(offsetR + 1, offsetC + 1), 5.5)
      assertEquals(tiled(offsetR + 1, offsetC + 2), 6.6)
    end for

end MatrixHelperTileSuite
