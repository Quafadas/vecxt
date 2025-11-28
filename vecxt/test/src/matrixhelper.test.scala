package vecxt

import munit.FunSuite
import narr.*
import all.*
import BoundsCheck.DoBoundsCheck.yes

class MatrixHelperSuite extends FunSuite:

  private def assertDiagonalMatrix(diagonal: NArray[Double], matrix: Matrix[Double])(using loc: munit.Location): Unit =
    val size = diagonal.length
    assertEquals(matrix.rows, size)
    assertEquals(matrix.cols, size)
    var row = 0
    while row < size do
      var col = 0
      while col < size do
        val expected = if row == col then diagonal(row) else 0.0
        assertEqualsDouble(matrix(row, col), expected, 1e-9, clue = s"entry ($row,$col)")
        col += 1
      end while
      row += 1
    end while
    assertEquals(matrix.raw.length, size * size)
  end assertDiagonalMatrix

  test("Matrix.diag size 1 populates 1x1 matrix"):
    val diagonal = NArray[Double](42.0)
    val diagMatrix = Matrix.createDiagonal(diagonal)
    assertDiagonalMatrix(diagonal, diagMatrix)

  test("Matrix.diag size 3 builds zero off-diagonal entries"):
    val diagonal = NArray[Double](1.0, -2.5, 7.75)
    val diagMatrix = Matrix.createDiagonal(diagonal)
    assertDiagonalMatrix(diagonal, diagMatrix)

  test("Matrix.diag size 7 handles longer vectors"):
    val diagonal = NArray.tabulate[Double](7)(i => (i + 1) * 0.5)
    val diagMatrix = Matrix.createDiagonal(diagonal)
    assertDiagonalMatrix(diagonal, diagMatrix)
end MatrixHelperSuite
