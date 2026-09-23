package vecxt_re

import vecxt.all.*

class LossMatrixSuite extends munit.FunSuite:

  test("attachProb is the fraction of outcomes (rows) with a positive portfolio loss") {
    // 4 outcomes x 3 transactions. Rows 1 and 3 attach; row 3 only through a single transaction.
    val losses = LossMatrix(
      Matrix.fromRows[Double](
        Array(0.0, 0.0, 0.0),
        Array(10.0, 0.0, 5.0),
        Array(0.0, 0.0, 0.0),
        Array(0.0, 0.0, 0.5)
      )
    )
    assertEqualsDouble(losses.attachProb, 0.5, 1e-12)
  }

  test("attachProb is 0 when nothing attaches and 1 when every outcome attaches") {
    assertEqualsDouble(LossMatrix(Matrix.zeros[Double]((7, 3))).attachProb, 0.0, 1e-12)
    assertEqualsDouble(LossMatrix(Matrix.fill(1.0, (7, 3))).attachProb, 1.0, 1e-12)
  }

  test("attachProb of a single-transaction portfolio") {
    val losses = LossMatrix(Matrix(Array(0.0, 1.0, 0.0, 2.0, 3.0), 5, 1))
    assertEqualsDouble(losses.attachProb, 3.0 / 5.0, 1e-12)
  }

  test("attachProb does not depend on storage order") {
    // Same logical matrix as the first test, stored row-major.
    val rowMajor = Matrix(
      Array(0.0, 0.0, 0.0, 10.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5),
      4,
      3,
      3,
      1,
      0
    )
    assertEqualsDouble(LossMatrix(rowMajor).attachProb, 0.5, 1e-12)
  }

  test("attachProb matches a naive count on a large sparse portfolio") {
    val rng = new scala.util.Random(42)
    val rows = 10007 // not a multiple of any SIMD width
    val cols = 5
    // ~90% of individual losses are zero, so a good share of outcomes don't attach at all.
    val raw = Array.fill(rows * cols)(if rng.nextDouble() < 0.9 then 0.0 else rng.nextDouble() * 100)
    val m = Matrix(raw, rows, cols)

    var attaching = 0
    for i <- 0 until rows do if (0 until cols).exists(j => m(i, j) > 0) then attaching += 1
    end for

    assert(attaching > 0 && attaching < rows)
    assertEqualsDouble(LossMatrix(m).attachProb, attaching.toDouble / rows, 1e-12)
  }

end LossMatrixSuite
