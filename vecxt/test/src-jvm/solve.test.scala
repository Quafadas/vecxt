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
import all.*
import BoundsCheck.DoBoundsCheck.yes

class SolveSuite extends FunSuite:

  val epsilon = 1e-10

  test("Identity matrix: Ax = b should give x = b") {
    val A = Matrix.eye[Double](3)
    val b = Array(1.0, 2.0, 3.0)
    val x = solve(A, b)

    for i <- 0 until 3 do assertEqualsDouble(x(i), b(i), epsilon)
    end for
  }

  test("Identity matrix with matrix b: Ax = b should give x = b") {
    val A = Matrix.eye[Double](3)
    val b = Matrix.zeros[Double](3, 2)
    b(0, 0) = 1.0
    b(1, 0) = 2.0
    b(2, 0) = 3.0
    b(0, 1) = 4.0
    b(1, 1) = 5.0
    b(2, 1) = 6.0

    val x = solve(A, b)

    for i <- 0 until 3; j <- 0 until 2 do assertEqualsDouble(x(i, j), b(i, j), epsilon)
    end for
  }

  test("Diagonal matrix") {
    // A = diag(2, 3, 4), b = [4, 9, 16], expected x = [2, 3, 4]
    val A = Matrix.zeros[Double](3, 3)
    A(0, 0) = 2.0
    A(1, 1) = 3.0
    A(2, 2) = 4.0

    val b = Array(4.0, 9.0, 16.0)
    val x = solve(A, b)

    assertEqualsDouble(x(0), 2.0, epsilon)
    assertEqualsDouble(x(1), 3.0, epsilon)
    assertEqualsDouble(x(2), 4.0, epsilon)
  }

  test("Simple 2x2 system") {
    // A = [2, 1; 1, 2], b = [5, 4], expected x = [2, 1]
    val A = Matrix.zeros[Double](2, 2)
    A(0, 0) = 2.0
    A(0, 1) = 1.0
    A(1, 0) = 1.0
    A(1, 1) = 2.0

    val b = Array(5.0, 4.0)
    val x = solve(A, b)

    assertEqualsDouble(x(0), 2.0, epsilon)
    assertEqualsDouble(x(1), 1.0, epsilon)
  }

  test("Simple 3x3 system") {
    // A = [1, 2, 3; 2, 5, 3; 1, 0, 8], b = [14, 18, 20]
    // Solution: x = [-92, 32, 14]
    val A = Matrix.zeros[Double](3, 3)
    A(0, 0) = 1.0
    A(0, 1) = 2.0
    A(0, 2) = 3.0
    A(1, 0) = 2.0
    A(1, 1) = 5.0
    A(1, 2) = 3.0
    A(2, 0) = 1.0
    A(2, 1) = 0.0
    A(2, 2) = 8.0

    val b = Array(14.0, 18.0, 20.0)
    val x = solve(A, b)

    // Verify A*x = b instead of checking specific values
    // (the system has a large solution that's sensitive to formulation)
    for i <- 0 until 3 do
      var sum = 0.0
      for j <- 0 until 3 do sum += A(i, j) * x(j)
      end for
      assertEqualsDouble(sum, b(i), epsilon)
    end for
  }

  test("Verify solution: A*x should equal b") {
    val A = Matrix.zeros[Double](3, 3)
    A(0, 0) = 3.0
    A(0, 1) = 1.0
    A(0, 2) = -1.0
    A(1, 0) = 2.0
    A(1, 1) = 4.0
    A(1, 2) = 1.0
    A(2, 0) = -1.0
    A(2, 1) = 2.0
    A(2, 2) = 5.0

    val b = Array(4.0, 13.0, 2.0)
    val x = solve(A, b)

    // Verify A*x = b
    for i <- 0 until 3 do
      var sum = 0.0
      for j <- 0 until 3 do sum += A(i, j) * x(j)
      end for
      assertEqualsDouble(sum, b(i), epsilon)
    end for
  }

  test("Symmetric positive definite matrix") {
    // A = [4, 2, 1; 2, 5, 2; 1, 2, 6], b = [7, 9, 9], solution exists
    val A = Matrix.zeros[Double](3, 3)
    A(0, 0) = 4.0
    A(0, 1) = 2.0
    A(0, 2) = 1.0
    A(1, 0) = 2.0
    A(1, 1) = 5.0
    A(1, 2) = 2.0
    A(2, 0) = 1.0
    A(2, 1) = 2.0
    A(2, 2) = 6.0

    val b = Array(7.0, 9.0, 9.0)
    val x = solve(A, b)

    // Verify A*x = b
    for i <- 0 until 3 do
      var sum = 0.0
      for j <- 0 until 3 do sum += A(i, j) * x(j)
      end for
      assertEqualsDouble(sum, b(i), 1e-9)
    end for
  }

  test("Upper triangular matrix") {
    val A = Matrix.zeros[Double](3, 3)
    A(0, 0) = 2.0
    A(0, 1) = 1.0
    A(0, 2) = 1.0
    A(1, 1) = 3.0
    A(1, 2) = 2.0
    A(2, 2) = 4.0

    val b = Array(8.0, 11.0, 12.0)
    val x = solve(A, b)

    // Verify A*x = b
    for i <- 0 until 3 do
      var sum = 0.0
      for j <- 0 until 3 do sum += A(i, j) * x(j)
      end for
      assertEqualsDouble(sum, b(i), epsilon)
    end for
  }

  test("Lower triangular matrix") {
    val A = Matrix.zeros[Double](3, 3)
    A(0, 0) = 2.0
    A(1, 0) = 1.0
    A(1, 1) = 3.0
    A(2, 0) = 1.0
    A(2, 1) = 2.0
    A(2, 2) = 4.0

    val b = Array(4.0, 10.0, 17.0)
    val x = solve(A, b)

    // Verify A*x = b
    for i <- 0 until 3 do
      var sum = 0.0
      for j <- 0 until 3 do sum += A(i, j) * x(j)
      end for
      assertEqualsDouble(sum, b(i), epsilon)
    end for
  }

  test("Multiple right-hand sides") {
    val A = Matrix.zeros[Double](3, 3)
    A(0, 0) = 1.0
    A(0, 1) = 2.0
    A(0, 2) = 3.0
    A(1, 0) = 2.0
    A(1, 1) = 5.0
    A(1, 2) = 3.0
    A(2, 0) = 1.0
    A(2, 1) = 0.0
    A(2, 2) = 8.0

    val b = Matrix.zeros[Double](3, 2)
    b(0, 0) = 14.0
    b(1, 0) = 18.0
    b(2, 0) = 20.0
    b(0, 1) = 6.0
    b(1, 1) = 9.0
    b(2, 1) = 9.0

    val x = solve(A, b)

    // Verify A*x = b for each column
    for col <- 0 until 2; i <- 0 until 3 do
      var sum = 0.0
      for j <- 0 until 3 do sum += A(i, j) * x(j, col)
      end for
      assertEqualsDouble(sum, b(i, col), epsilon)
    end for
  }

  test("Single element matrix") {
    val A = Matrix.zeros[Double](1, 1)
    A(0, 0) = 5.0

    val b = Array(15.0)
    val x = solve(A, b)

    assertEqualsDouble(x(0), 3.0, epsilon)
  }

  test("Large well-conditioned system") {
    val n = 20
    val A = Matrix.eye[Double](n)
    // Add some off-diagonal elements
    for i <- 0 until n - 1 do A(i, i + 1) = 0.1
    end for

    val b = Array.fill(n)(1.0)
    val x = solve(A, b)

    // Verify A*x = b
    for i <- 0 until n do
      var sum = 0.0
      for j <- 0 until n do sum += A(i, j) * x(j)
      end for
      assertEqualsDouble(sum, b(i), 1e-8)
    end for
  }

  test("Permutation matrix (tests pivoting)") {
    // Permutation that swaps rows - tests that pivoting works
    val A = Matrix.zeros[Double](3, 3)
    A(0, 2) = 1.0
    A(1, 0) = 1.0
    A(2, 1) = 1.0

    val b = Array(3.0, 1.0, 2.0)
    val x = solve(A, b)

    assertEqualsDouble(x(0), 1.0, epsilon)
    assertEqualsDouble(x(1), 2.0, epsilon)
    assertEqualsDouble(x(2), 3.0, epsilon)
  }

  test("Singular matrix should throw ArithmeticException") {
    val A = Matrix.zeros[Double](3, 3)
    A(0, 0) = 1.0
    A(0, 1) = 2.0
    A(0, 2) = 3.0
    A(1, 0) = 2.0
    A(1, 1) = 4.0
    A(1, 2) = 6.0
    A(2, 0) = 1.0
    A(2, 1) = 1.0
    A(2, 2) = 1.0

    val b = Array(1.0, 2.0, 3.0)

    intercept[ArithmeticException] {
      solve(A, b)
    }
  }

  test("Non-square matrix should throw exception") {
    val A = Matrix.zeros[Double](3, 2)
    val b = Array(1.0, 2.0, 3.0)

    intercept[Exception] {
      solve(A, b)
    }
  }

  test("Mismatched dimensions should throw exception when bounds checking enabled") {
    val A = Matrix.zeros[Double](3, 3)
    val b = Array(1.0, 2.0) // Wrong size

    intercept[MatrixDimensionMismatch] {
      solve(A, b)(using yes)
    }
  }

  test("NaN in matrix A should throw exception when bounds checking enabled") {
    val A = Matrix.zeros[Double](2, 2)
    A(0, 0) = Double.NaN
    A(1, 1) = 1.0
    val b = Array(1.0, 2.0)

    intercept[IllegalArgumentException] {
      solve(A, b)(using yes)
    }
  }

  test("NaN in vector b should throw exception when bounds checking enabled") {
    val A = Matrix.eye[Double](2)
    val b = Array(Double.NaN, 2.0)

    intercept[IllegalArgumentException] {
      solve(A, b)(using yes)
    }
  }

  test("Scaled identity gives scaled inverse solution") {
    val scale = 2.5
    val A = Matrix.eye[Double](3)
    for i <- 0 until 3 do A(i, i) = scale
    end for

    val b = Array(5.0, 10.0, 15.0)
    val x = solve(A, b)

    assertEqualsDouble(x(0), 2.0, epsilon)
    assertEqualsDouble(x(1), 4.0, epsilon)
    assertEqualsDouble(x(2), 6.0, epsilon)
  }

  test("Solve with zero right-hand side") {
    val A = Matrix.zeros[Double](3, 3)
    A(0, 0) = 1.0
    A(0, 1) = 2.0
    A(0, 2) = 3.0
    A(1, 0) = 2.0
    A(1, 1) = 5.0
    A(1, 2) = 3.0
    A(2, 0) = 1.0
    A(2, 1) = 0.0
    A(2, 2) = 8.0

    val b = Array(0.0, 0.0, 0.0)
    val x = solve(A, b)

    // Solution should be all zeros
    for i <- 0 until 3 do assertEqualsDouble(x(i), 0.0, epsilon)
    end for
  }

  test("Random well-conditioned matrices") {
    val rand = scala.util.Random(42)
    for trial <- 0 until 10 do
      val n = 4
      val A = Matrix.zeros[Double](n, n)
      // Create a well-conditioned matrix by starting with diagonal dominance
      for i <- 0 until n do
        A(i, i) = 5.0 + rand.nextDouble() * 5.0
        for j <- 0 until n if i != j do A(i, j) = rand.nextDouble() * 0.5
        end for
      end for

      val b = Array.fill(n)(rand.nextDouble() * 10.0)
      val x = solve(A, b)

      // Verify A*x ≈ b
      for i <- 0 until n do
        var sum = 0.0
        for j <- 0 until n do sum += A(i, j) * x(j)
        end for
        assertEqualsDouble(sum, b(i), 1e-9)
      end for
    end for
  }

  test("Solve preserves original matrices (no mutation)") {
    val A = Matrix.zeros[Double](2, 2)
    A(0, 0) = 2.0
    A(0, 1) = 1.0
    A(1, 0) = 1.0
    A(1, 1) = 2.0

    val b = Array(3.0, 3.0)

    val AOrig = A.deepCopy
    val bOrig = b.clone()

    solve(A, b)

    // Check A is unchanged
    for i <- 0 until 2; j <- 0 until 2 do assertEqualsDouble(A(i, j), AOrig(i, j), epsilon)
    end for

    // Check b is unchanged
    for i <- 0 until 2 do assertEqualsDouble(b(i), bOrig(i), epsilon)
    end for
  }

  test("Tridiagonal system") {
    // Tests a common structured matrix type
    val n = 5
    val A = Matrix.zeros[Double](n, n)
    for i <- 0 until n do
      A(i, i) = 2.0
      if i > 0 then A(i, i - 1) = -1.0
      end if
      if i < n - 1 then A(i, i + 1) = -1.0
      end if
    end for

    val b = Array.fill(n)(1.0)
    val x = solve(A, b)

    // Verify solution
    for i <- 0 until n do
      var sum = 0.0
      for j <- 0 until n do sum += A(i, j) * x(j)
      end for
      assertEqualsDouble(sum, b(i), epsilon)
    end for
  }

  test("Hilbert matrix (ill-conditioned but solvable for small n)") {
    // Hilbert matrices are notoriously ill-conditioned but should still work for n=3
    val n = 3
    val A = Matrix.zeros[Double](n, n)
    for i <- 0 until n; j <- 0 until n do A(i, j) = 1.0 / (i + j + 1.0)
    end for

    val b = Array.fill(n)(1.0)
    val x = solve(A, b)

    // Verify solution (use slightly larger tolerance due to conditioning)
    for i <- 0 until n do
      var sum = 0.0
      for j <- 0 until n do sum += A(i, j) * x(j)
      end for
      assertEqualsDouble(sum, b(i), 1e-8)
    end for
  }

end SolveSuite
