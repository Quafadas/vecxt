/*
 * Copyright 2023 quafadas
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
import narr.*
import all.*
import BoundsCheck.DoBoundsCheck.yes

class DeterminantSuite extends FunSuite:

  private val tolerance = 1e-10

  test("determinant of 1x1 matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(5.0)
    )
    assertEqualsDouble(mat.det, 5.0, tolerance)
  }

  test("determinant of 2x2 matrix") {
    // | 3  8 |
    // | 4  6 |
    // det = 3*6 - 8*4 = 18 - 32 = -14
    val mat = Matrix.fromRows[Double](
      NArray(3.0, 8.0),
      NArray(4.0, 6.0)
    )
    assertEqualsDouble(mat.det, -14.0, tolerance)
  }

  test("determinant of 2x2 identity matrix") {
    val mat = Matrix.eye[Double](2)
    assertEqualsDouble(mat.det, 1.0, tolerance)
  }

  test("determinant of 3x3 identity matrix") {
    val mat = Matrix.eye[Double](3)
    assertEqualsDouble(mat.det, 1.0, tolerance)
  }

  test("determinant of 3x3 matrix") {
    // | 6  1  1 |
    // | 4 -2  5 |
    // | 2  8  7 |
    // det = 6*(-2*7 - 5*8) - 1*(4*7 - 5*2) + 1*(4*8 - (-2)*2)
    //     = 6*(-14 - 40) - 1*(28 - 10) + 1*(32 + 4)
    //     = 6*(-54) - 1*(18) + 1*(36)
    //     = -324 - 18 + 36
    //     = -306
    val mat = Matrix.fromRows[Double](
      NArray(6.0, 1.0, 1.0),
      NArray(4.0, -2.0, 5.0),
      NArray(2.0, 8.0, 7.0)
    )
    assertEqualsDouble(mat.det, -306.0, tolerance)
  }

  test("determinant of 3x3 singular matrix (zero determinant)") {
    // | 1  2  3 |
    // | 4  5  6 |
    // | 7  8  9 |
    // This matrix is singular (rows are linearly dependent)
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(4.0, 5.0, 6.0),
      NArray(7.0, 8.0, 9.0)
    )
    assertEqualsDouble(mat.det, 0.0, tolerance)
  }

  test("determinant of 4x4 identity matrix") {
    val mat = Matrix.eye[Double](4)
    assertEqualsDouble(mat.det, 1.0, tolerance)
  }

  test("determinant of 4x4 matrix") {
    // | 5  7  6  5 |
    // | 7  10 8  7 |
    // | 6  8  10 9 |
    // | 5  7  9  10|
    // Expected determinant: 1
    val mat = Matrix.fromRows[Double](
      NArray(5.0, 7.0, 6.0, 5.0),
      NArray(7.0, 10.0, 8.0, 7.0),
      NArray(6.0, 8.0, 10.0, 9.0),
      NArray(5.0, 7.0, 9.0, 10.0)
    )
    assertEqualsDouble(mat.det, 1.0, tolerance)
  }

  test("determinant of upper triangular matrix") {
    // | 3  6  9  |
    // | 0  2  4  |
    // | 0  0  5  |
    // det = 3 * 2 * 5 = 30
    val mat = Matrix.fromRows[Double](
      NArray(3.0, 6.0, 9.0),
      NArray(0.0, 2.0, 4.0),
      NArray(0.0, 0.0, 5.0)
    )
    assertEqualsDouble(mat.det, 30.0, tolerance)
  }

  test("determinant of lower triangular matrix") {
    // | 4  0  0  |
    // | 2  3  0  |
    // | 1  5  6  |
    // det = 4 * 3 * 6 = 72
    val mat = Matrix.fromRows[Double](
      NArray(4.0, 0.0, 0.0),
      NArray(2.0, 3.0, 0.0),
      NArray(1.0, 5.0, 6.0)
    )
    assertEqualsDouble(mat.det, 72.0, tolerance)
  }

  test("determinant of diagonal matrix") {
    // | 2  0  0  |
    // | 0  3  0  |
    // | 0  0  4  |
    // det = 2 * 3 * 4 = 24
    val mat = Matrix.fromRows[Double](
      NArray(2.0, 0.0, 0.0),
      NArray(0.0, 3.0, 0.0),
      NArray(0.0, 0.0, 4.0)
    )
    assertEqualsDouble(mat.det, 24.0, tolerance)
  }

  test("determinant of matrix with row of zeros") {
    // | 1  2  3  |
    // | 0  0  0  |
    // | 4  5  6  |
    // det = 0 (row of zeros)
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(0.0, 0.0, 0.0),
      NArray(4.0, 5.0, 6.0)
    )
    assertEqualsDouble(mat.det, 0.0, tolerance)
  }

  test("determinant throws exception for non-square matrix") {
    val mat = Matrix[Double](NArray(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), (2, 3))
    intercept[IllegalArgumentException] {
      mat.det
    }
  }

  test("determinant of negative values") {
    // | -2  1  |
    // |  3 -4  |
    // det = (-2)*(-4) - 1*3 = 8 - 3 = 5
    val mat = Matrix.fromRows[Double](
      NArray(-2.0, 1.0),
      NArray(3.0, -4.0)
    )
    assertEqualsDouble(mat.det, 5.0, tolerance)
  }

  test("determinant of 5x5 matrix") {
    // A more complex case
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0, 4.0, 5.0),
      NArray(2.0, 3.0, 4.0, 5.0, 6.0),
      NArray(3.0, 4.0, 5.0, 6.0, 7.0),
      NArray(4.0, 5.0, 6.0, 7.0, 8.0),
      NArray(5.0, 6.0, 7.0, 8.0, 9.0)
    )
    // This is a singular matrix with all rows linearly dependent
    assertEqualsDouble(mat.det, 0.0, tolerance)
  }

  // Adjugate matrix tests
  test("adjugate of 1x1 matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(5.0)
    )
    val adj = mat.adj
    assertEqualsDouble(adj(0, 0), 1.0, tolerance)
  }

  test("adjugate of 2x2 matrix") {
    // | 3  8 |        | 6  -8 |
    // | 4  6 |  adj = |-4   3 |
    val mat = Matrix.fromRows[Double](
      NArray(3.0, 8.0),
      NArray(4.0, 6.0)
    )
    val adj = mat.adj
    assertEqualsDouble(adj(0, 0), 6.0, tolerance)
    assertEqualsDouble(adj(0, 1), -8.0, tolerance)
    assertEqualsDouble(adj(1, 0), -4.0, tolerance)
    assertEqualsDouble(adj(1, 1), 3.0, tolerance)
  }

  test("adjugate of 2x2 identity matrix") {
    val mat = Matrix.eye[Double](2)
    val adj = mat.adj
    assertEqualsDouble(adj(0, 0), 1.0, tolerance)
    assertEqualsDouble(adj(0, 1), 0.0, tolerance)
    assertEqualsDouble(adj(1, 0), 0.0, tolerance)
    assertEqualsDouble(adj(1, 1), 1.0, tolerance)
  }

  test("adjugate of 3x3 matrix") {
    // | 1  2  3 |
    // | 0  4  5 |
    // | 1  0  6 |
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(0.0, 4.0, 5.0),
      NArray(1.0, 0.0, 6.0)
    )
    val adj = mat.adj

    // Verify A * adj(A) = det(A) * I
    val det = mat.det
    val product = mat @@ adj
    val identity = Matrix.eye[Double](3)

    for
      i <- 0 until 3
      j <- 0 until 3
    do assertEqualsDouble(product(i, j), det * identity(i, j), 1e-9, s"Mismatch at ($i,$j)")
    end for
  }

  test("adjugate of 3x3 identity matrix") {
    val mat = Matrix.eye[Double](3)
    val adj = mat.adj
    // Adjugate of identity is identity
    for
      i <- 0 until 3
      j <- 0 until 3
    do assertEqualsDouble(adj(i, j), mat(i, j), tolerance)
    end for
  }

  test("adjugate satisfies A * adj(A) = det(A) * I for 4x4 matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0, 4.0),
      NArray(2.0, 1.0, 4.0, 3.0),
      NArray(3.0, 4.0, 1.0, 2.0),
      NArray(4.0, 3.0, 2.0, 1.0)
    )
    val adj = mat.adj
    val det = mat.det
    val product = mat @@ adj
    val identity = Matrix.eye[Double](4)

    for
      i <- 0 until 4
      j <- 0 until 4
    do assertEqualsDouble(product(i, j), det * identity(i, j), 1e-8, s"Mismatch at ($i,$j)")
    end for
  }

  test("adjugate of diagonal matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(2.0, 0.0, 0.0),
      NArray(0.0, 3.0, 0.0),
      NArray(0.0, 0.0, 4.0)
    )
    val adj = mat.adj
    // For diagonal matrix, adj is diagonal with reciprocal products
    assertEqualsDouble(adj(0, 0), 12.0, tolerance) // 3*4
    assertEqualsDouble(adj(1, 1), 8.0, tolerance) // 2*4
    assertEqualsDouble(adj(2, 2), 6.0, tolerance) // 2*3

    // Off-diagonal should be zero
    for
      i <- 0 until 3
      j <- 0 until 3
      if i != j
    do assertEqualsDouble(adj(i, j), 0.0, tolerance)
    end for
  }

  test("adjugate throws exception for non-square matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(4.0, 5.0, 6.0)
    )
    intercept[IllegalArgumentException] {
      mat.adj
    }
  }

  test("adjugate of singular matrix") {
    // Singular matrix (det = 0)
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(2.0, 4.0, 6.0),
      NArray(3.0, 6.0, 9.0)
    )
    val adj = mat.adj
    val det = mat.det
    assertEqualsDouble(det, 0.0, tolerance, "Determinant should be zero")

    // For singular matrix, A * adj(A) = 0 matrix (since det = 0)
    val product = mat @@ adj
    for
      i <- 0 until 3
      j <- 0 until 3
    do assertEqualsDouble(product(i, j), 0.0, 1e-9, s"Product should be zero at ($i,$j)")
    end for
  }

  // Matrix inverse tests
  test("inverse of 1x1 matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(5.0)
    )
    val inv = mat.inv
    assertEqualsDouble(inv(0, 0), 0.2, tolerance, "1/5 = 0.2")
  }

  test("inverse of 2x2 matrix") {
    // | 3  8 |        | -3/7   4/7 |
    // | 4  6 |  inv = | 2/7   -3/14|
    val mat = Matrix.fromRows[Double](
      NArray(3.0, 8.0),
      NArray(4.0, 6.0)
    )
    val inv = mat.inv
    assertEqualsDouble(inv(0, 0), -3.0 / 7.0, tolerance, "Element (0,0)")
    assertEqualsDouble(inv(0, 1), 4.0 / 7.0, tolerance, "Element (0,1)")
    assertEqualsDouble(inv(1, 0), 2.0 / 7.0, tolerance, "Element (1,0)")
    assertEqualsDouble(inv(1, 1), -3.0 / 14.0, tolerance, "Element (1,1)")
  }

  test("inverse of 2x2 identity matrix") {
    val mat = Matrix.eye[Double](2)
    val inv = mat.inv
    // Inverse of identity is identity
    for
      i <- 0 until 2
      j <- 0 until 2
    do assertEqualsDouble(inv(i, j), mat(i, j), tolerance, s"Identity inverse at ($i,$j)")
    end for
  }

  test("inverse satisfies A * A⁻¹ = I for 3x3 matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(0.0, 4.0, 5.0),
      NArray(1.0, 0.0, 6.0)
    )
    val inv = mat.inv
    val product = mat @@ inv
    val identity = Matrix.eye[Double](3)

    for
      i <- 0 until 3
      j <- 0 until 3
    do assertEqualsDouble(product(i, j), identity(i, j), 1e-9, s"A * A⁻¹ = I at ($i,$j)")
    end for
  }

  test("inverse satisfies A⁻¹ * A = I for 3x3 matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(0.0, 4.0, 5.0),
      NArray(1.0, 0.0, 6.0)
    )
    val inv = mat.inv
    val product = inv @@ mat
    val identity = Matrix.eye[Double](3)

    for
      i <- 0 until 3
      j <- 0 until 3
    do assertEqualsDouble(product(i, j), identity(i, j), 1e-9, s"A⁻¹ * A = I at ($i,$j)")
    end for
  }

  test("inverse of 4x4 matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(4.0, 7.0, 2.0, 3.0),
      NArray(3.0, 6.0, 1.0, 2.0),
      NArray(2.0, 5.0, 3.0, 1.0),
      NArray(1.0, 4.0, 2.0, 4.0)
    )
    val inv = mat.inv
    val product = mat @@ inv
    val identity = Matrix.eye[Double](4)

    for
      i <- 0 until 4
      j <- 0 until 4
    do assertEqualsDouble(product(i, j), identity(i, j), 1e-8, s"Product at ($i,$j)")
    end for
  }

  test("inverse of diagonal matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(2.0, 0.0, 0.0),
      NArray(0.0, 3.0, 0.0),
      NArray(0.0, 0.0, 4.0)
    )
    val inv = mat.inv

    // Inverse of diagonal matrix has reciprocal diagonal elements
    assertEqualsDouble(inv(0, 0), 0.5, tolerance, "1/2")
    assertEqualsDouble(inv(1, 1), 1.0 / 3.0, tolerance, "1/3")
    assertEqualsDouble(inv(2, 2), 0.25, tolerance, "1/4")

    // Off-diagonal should be zero
    for
      i <- 0 until 3
      j <- 0 until 3
      if i != j
    do assertEqualsDouble(inv(i, j), 0.0, tolerance, s"Off-diagonal at ($i,$j)")
    end for
  }

  test("inverse throws exception for non-square matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(4.0, 5.0, 6.0)
    )
    intercept[IllegalArgumentException] {
      mat.inv
    }
  }

  test("inverse throws exception for singular matrix") {
    // Singular matrix (det = 0)
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(2.0, 4.0, 6.0),
      NArray(3.0, 6.0, 9.0)
    )
    intercept[ArithmeticException] {
      mat.inv
    }
  }

  test("double inverse returns original matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0),
      NArray(3.0, 4.0)
    )
    val inv = mat.inv
    val doubleInv = inv.inv

    for
      i <- 0 until 2
      j <- 0 until 2
    do assertEqualsDouble(doubleInv(i, j), mat(i, j), 1e-9, s"(A⁻¹)⁻¹ = A at ($i,$j)")
    end for
  }

  test("A * A.inv ≈ I for random invertible 5x5 matrix") {
    val mat = Matrix.rand(5, 5)
    if math.abs(mat.det) > 1e-8 then
      val inv = mat.inv
      val product = mat @@ inv
      val identity = Matrix.eye[Double](5)
      for i <- 0 until 5; j <- 0 until 5 do assertEqualsDouble(product(i, j), identity(i, j), 1e-6)
      end for
    end if
  }

  test("adjugate and inverse relationship") {
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(0.0, 4.0, 5.0),
      NArray(1.0, 0.0, 6.0)
    )
    val det = mat.det
    val adj = mat.adj
    val inv = mat.inv

    val scaledAdj = adj / det
    for i <- 0 until 3; j <- 0 until 3 do assertEqualsDouble(scaledAdj(i, j), inv(i, j), 1e-9, s"Mismatch at ($i,$j)")
    end for
  }

end DeterminantSuite
