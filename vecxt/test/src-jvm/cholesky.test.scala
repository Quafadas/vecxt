/*
 * Copyright 2025 quafadas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package vecxt

import munit.FunSuite

import all.*
import vecxt.BoundsCheck.BoundsCheck

class CholeskySuite extends FunSuite:
  inline given bc: BoundsCheck = true
  private val tolerance = 1e-9

  test("cholesky of 1x1 positive scalar") {

    val a = Matrix.fromRows[Double](
      Array(4.0)
    )

    val L = cholesky(a)

    assertEquals(L.rows, 1)
    assertEquals(L.cols, 1)
    assertEqualsDouble(L(0, 0), 2.0, tolerance)
  }

  test("random SPD") {

    val M = Matrix.rand(10, 10)
    val A = M @@ M.T
    val L = cholesky(A)
    assertMatrixEquals(L @@ L.T, A)
  }

  test("cholesky of 2x2 SPD matrix: L * L^T ≈ A") {
    // A is symmetric positive definite
    // | 4  2 |
    // | 2  3 |

    val a = Matrix.fromRows[Double](
      Array(4.0, 2.0),
      Array(2.0, 3.0)
    )

    val L = cholesky(a)

    // L should be lower-triangular with positive diagonal
    assertEqualsDouble(L(0, 1), 0.0, tolerance)
    assert(L(0, 0) > 0.0)
    assert(L(1, 1) > 0.0)

    // Reconstruct A via L * L^T and compare element-wise
    val reconstructed = L @@ L.T

    for
      i <- 0 until a.rows
      j <- 0 until a.cols
    do assertEqualsDouble(reconstructed(i, j), a(i, j), tolerance, clue = s"at ($i,$j)")
    end for
  }

  test("cholesky of 3x3 SPD matrix: L * L^T ≈ A") {
    // Construct A = B * B^T where B is random-ish but small integers

    val B = Matrix.fromRows[Double](
      Array(1.0, 2.0, 3.0),
      Array(0.0, 1.0, 4.0),
      Array(5.0, 6.0, 0.0)
    )

    val A = B @@ B.T

    val L = cholesky(A)

    // A should be reconstructed (within tolerance) from L * L^T
    val reconstructed = L @@ L.T

    for
      i <- 0 until A.rows
      j <- 0 until A.cols
    do assertEqualsDouble(reconstructed(i, j), A(i, j), tolerance, clue = s"at ($i,$j)")
    end for
  }

  test("cholesky throws for non-symmetric matrix") {
    // Not symmetric: (0,1) != (1,0)

    val a = Matrix.fromRows[Double](
      Array(1.0, 2.0),
      Array(3.0, 4.0)
    )

    intercept[MatrixNotSymmetricException] {
      cholesky(a)
    }
  }

  test("cholesky throws for non-positive-definite matrix (singular)") {
    // Symmetric but not positive definite (determinant is zero)
    // | 1  2 |
    // | 2  4 |

    val a = Matrix.fromRows[Double](
      Array(1.0, 2.0),
      Array(2.0, 4.0)
    )

    intercept[ArithmeticException] {
      cholesky(a)
    }
  }

  test("cholesky is lower-triangular: upper entries are zero") {
    // A simple 3x3 SPD matrix

    val a = Matrix.fromRows[Double](
      Array(25.0, 15.0, -5.0),
      Array(15.0, 18.0, 0.0),
      Array(-5.0, 0.0, 11.0)
    )

    val L = cholesky(a)

    for
      i <- 0 until L.rows
      j <- 0 until L.cols
      if j > i
    do assertEqualsDouble(L(i, j), 0.0, tolerance, clue = s"upper entry at ($i,$j) should be zero")
    end for
  }
end CholeskySuite
