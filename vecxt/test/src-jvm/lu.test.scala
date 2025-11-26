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
import vecxt.DoubleMatrix.matmul

class LUSuite extends FunSuite:

  val epsilon = 1e-10

  /** Helper to apply permutation array to matrix rows. Constructs permutation matrix P from pivot array and multiplies:
    * P * A
    */
  def applyPermutation(m: Matrix[Double], pivots: Array[Int]): Matrix[Double] =
    val result = Matrix.zeros[Double](m.rows, m.cols)
    // LAPACK pivots represent successive row swaps
    // We need to apply them in order to get the final permutation
    val rowOrder = Array.tabulate(m.rows)(identity)

    for i <- 0 until pivots.length do
      // Swap rows i and pivots(i) in the row order
      val temp = rowOrder(i)
      rowOrder(i) = rowOrder(pivots(i))
      rowOrder(pivots(i)) = temp
    end for

    // Apply the permutation
    for i <- 0 until m.rows do
      for j <- 0 until m.cols do result(i, j) = m(rowOrder(i), j)
      end for
    end for

    result
  end applyPermutation

  /** Helper to verify L is lower triangular with unit diagonal */
  def isLowerUnitTriangular(m: Matrix[Double], tol: Double = epsilon): Boolean =
    if m.rows < m.cols then return false
    end if

    for i <- 0 until m.rows do
      for j <- 0 until m.cols do
        if i < j then
          // Above diagonal should be zero
          if math.abs(m(i, j)) > tol then return false
        else if i == j then
          // Diagonal should be one
          if math.abs(m(i, j) - 1.0) > tol then return false
        end if
      end for
    end for
    true
  end isLowerUnitTriangular

  /** Helper to verify U is upper triangular */
  def isUpperTriangular(m: Matrix[Double], tol: Double = epsilon): Boolean =
    if m.rows > m.cols then return false
    end if

    for i <- 0 until m.rows do
      for j <- 0 until m.cols do
        if i > j then
          // Below diagonal should be zero
          if math.abs(m(i, j)) > tol then return false
        end if
      end for
    end for
    true
  end isUpperTriangular

  /** Helper to check if two matrices are approximately equal */
  def matricesEqual(a: Matrix[Double], b: Matrix[Double], tol: Double = epsilon): Boolean =
    if a.rows != b.rows || a.cols != b.cols then return false
    end if

    for i <- 0 until a.rows do
      for j <- 0 until a.cols do
        if math.abs(a(i, j) - b(i, j)) > tol then
          println(s"Mismatch at ($i, $j): ${a(i, j)} vs ${b(i, j)}, diff = ${math.abs(a(i, j) - b(i, j))}")
          return false
        end if
      end for
    end for
    true
  end matricesEqual

  test("LU decomposition of identity matrix") {
    val id = Matrix.eye[Double](3)
    val luResult = lu(id)

    // For identity matrix: L = I, U = I, no pivoting needed
    assert(isLowerUnitTriangular(luResult.L), "L should be lower triangular with unit diagonal")
    assert(isUpperTriangular(luResult.U), "U should be upper triangular")

    // U should equal identity
    for i <- 0 until 3 do
      for j <- 0 until 3 do
        val expected = if i == j then 1.0 else 0.0
        assertEqualsDouble(luResult.U(i, j), expected, epsilon)
      end for
    end for

    // Verify P*A = L*U
    val PA = applyPermutation(id, luResult.P)
    val LU = luResult.L.matmul(luResult.U)(using false)
    assert(matricesEqual(PA, LU), "P*A should equal L*U")
  }

  test("LU decomposition of diagonal matrix") {
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 2.0
    m(1, 1) = 3.0
    m(2, 2) = 5.0

    val luResult = lu(m)

    assert(isLowerUnitTriangular(luResult.L), "L should be lower triangular with unit diagonal")
    assert(isUpperTriangular(luResult.U), "U should be upper triangular")

    // Verify P*A = L*U
    val PA = applyPermutation(m, luResult.P)
    val LU = luResult.L.matmul(luResult.U)(using false)
    assert(matricesEqual(PA, LU, 1e-9), "P*A should equal L*U")

    // For diagonal matrix with no zeros, U diagonal should match (possibly reordered)
    val uDiag = Array.tabulate(3)(i => luResult.U(i, i))
    val sortedUDiag = uDiag.sorted.reverse
    val expectedDiag = Array(5.0, 3.0, 2.0)
    for i <- 0 until 3 do assertEqualsDouble(sortedUDiag(i), expectedDiag(i), epsilon)
    end for
  }

  test("LU decomposition of 2x2 matrix") {
    // Matrix: [1, 2]
    //         [3, 4]
    val m = Matrix.zeros[Double](2, 2)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(1, 0) = 3.0
    m(1, 1) = 4.0

    val luResult = lu(m)

    assert(isLowerUnitTriangular(luResult.L), "L should be lower triangular with unit diagonal")
    assert(isUpperTriangular(luResult.U), "U should be upper triangular")

    // Verify P*A = L*U
    val PA = applyPermutation(m, luResult.P)
    val LU = luResult.L.matmul(luResult.U)(using false)
    assert(matricesEqual(PA, LU, 1e-9), "P*A should equal L*U")
  }

  test("LU decomposition of 3x3 matrix with pivoting") {
    // Matrix requiring pivoting
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(0, 2) = 3.0
    m(1, 0) = 4.0
    m(1, 1) = 5.0
    m(1, 2) = 6.0
    m(2, 0) = 7.0
    m(2, 1) = 8.0
    m(2, 2) = 10.0

    val luResult = lu(m)

    assert(isLowerUnitTriangular(luResult.L), "L should be lower triangular with unit diagonal")
    assert(isUpperTriangular(luResult.U), "U should be upper triangular")

    // Verify P*A = L*U
    val PA = applyPermutation(m, luResult.P)
    val LU = luResult.L.matmul(luResult.U)(using false)
    assert(matricesEqual(PA, LU, 1e-9), "P*A should equal L*U")
  }

  test("LU decomposition preserves determinant magnitude") {
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 2.0
    m(0, 1) = 1.0
    m(0, 2) = 3.0
    m(1, 0) = 1.0
    m(1, 1) = 3.0
    m(1, 2) = 2.0
    m(2, 0) = 3.0
    m(2, 1) = 2.0
    m(2, 2) = 1.0

    val luResult = lu(m)

    // Determinant of original matrix
    val detA = m.det

    // Determinant from LU: det(A) = ±det(U) (since det(L) = 1)
    // The sign depends on the number of row swaps
    val detU = (0 until 3).map(i => luResult.U(i, i)).product

    // Should match in absolute value
    assertEqualsDouble(math.abs(detA), math.abs(detU), epsilon)
  }

  test("LU decomposition of rectangular matrix (tall)") {
    // 4x3 matrix (more rows than columns)
    val m = Matrix.zeros[Double](4, 3)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(0, 2) = 3.0
    m(1, 0) = 4.0
    m(1, 1) = 5.0
    m(1, 2) = 6.0
    m(2, 0) = 7.0
    m(2, 1) = 8.0
    m(2, 2) = 9.0
    m(3, 0) = 10.0
    m(3, 1) = 11.0
    m(3, 2) = 12.0

    val luResult = lu(m)

    // L should be 4x3, U should be 3x3
    assertEquals(luResult.L.rows, 4)
    assertEquals(luResult.L.cols, 3)
    assertEquals(luResult.U.rows, 3)
    assertEquals(luResult.U.cols, 3)

    assert(isLowerUnitTriangular(luResult.L), "L should be lower triangular with unit diagonal")
    assert(isUpperTriangular(luResult.U), "U should be upper triangular")

    // Verify P*A = L*U
    val PA = applyPermutation(m, luResult.P)
    val LU = luResult.L.matmul(luResult.U)(using false)
    assert(matricesEqual(PA, LU, 1e-9), "P*A should equal L*U")
  }

  test("LU decomposition of rectangular matrix (wide)") {
    // 3x4 matrix (more columns than rows)
    val m = Matrix.zeros[Double](3, 4)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(0, 2) = 3.0
    m(0, 3) = 4.0
    m(1, 0) = 5.0
    m(1, 1) = 6.0
    m(1, 2) = 7.0
    m(1, 3) = 8.0
    m(2, 0) = 9.0
    m(2, 1) = 10.0
    m(2, 2) = 11.0
    m(2, 3) = 12.0

    val luResult = lu(m)

    // L should be 3x3, U should be 3x4
    assertEquals(luResult.L.rows, 3)
    assertEquals(luResult.L.cols, 3)
    assertEquals(luResult.U.rows, 3)
    assertEquals(luResult.U.cols, 4)

    assert(isLowerUnitTriangular(luResult.L), "L should be lower triangular with unit diagonal")
    assert(isUpperTriangular(luResult.U), "U should be upper triangular")

    // Verify P*A = L*U
    val PA = applyPermutation(m, luResult.P)
    val LU = luResult.L.matmul(luResult.U)(using false)
    assert(matricesEqual(PA, LU, 1e-9), "P*A should equal L*U")
  }

  test("LU decomposition of singular matrix") {
    // Singular matrix (rank 2)
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(0, 2) = 3.0
    m(1, 0) = 2.0
    m(1, 1) = 4.0
    m(1, 2) = 6.0
    m(2, 0) = 4.0
    m(2, 1) = 5.0
    m(2, 2) = 6.0

    val luResult = lu(m)

    // Should still return valid L and U
    assert(isLowerUnitTriangular(luResult.L), "L should be lower triangular with unit diagonal")
    assert(isUpperTriangular(luResult.U), "U should be upper triangular")

    // For singular matrix, at least one diagonal element of U should be (near) zero
    val uDiag = Array.tabulate(3)(i => luResult.U(i, i))
    val hasZeroDiag = uDiag.exists(d => math.abs(d) < epsilon)
    assert(hasZeroDiag, "Singular matrix should have at least one zero diagonal in U")

    // Verify P*A = L*U still holds
    val PA = applyPermutation(m, luResult.P)
    val LU = luResult.L.matmul(luResult.U)(using false)
    assert(matricesEqual(PA, LU, 1e-9), "P*A should equal L*U even for singular matrix")
  }

  test("LU decomposition with random symmetric matrix") {
    val rand = scala.util.Random(42)
    val n = 5
    val m = Matrix.zeros[Double](n, n)

    // Create symmetric positive definite matrix
    for i <- 0 until n; j <- i until n do
      val v = rand.nextDouble() * 2 - 1
      m(i, j) = v
      m(j, i) = v
    end for
    // Add to diagonal to ensure positive definiteness
    for i <- 0 until n do m(i, i) = m(i, i) + n
    end for

    val luResult = lu(m)

    assert(isLowerUnitTriangular(luResult.L), "L should be lower triangular with unit diagonal")
    assert(isUpperTriangular(luResult.U), "U should be upper triangular")

    // Verify P*A = L*U
    val PA = applyPermutation(m, luResult.P)
    val LU = luResult.L.matmul(luResult.U)(using false)
    assert(matricesEqual(PA, LU, 1e-8), "P*A should equal L*U")
  }

  test("LU decomposition L diagonal is all ones") {
    val m = Matrix.zeros[Double](4, 4)
    val rand = scala.util.Random(123)
    for i <- 0 until 4; j <- 0 until 4 do m(i, j) = rand.nextDouble() * 10
    end for

    val luResult = lu(m)

    // Check all diagonal elements of L are 1.0
    for i <- 0 until math.min(luResult.L.rows, luResult.L.cols) do assertEqualsDouble(luResult.L(i, i), 1.0, epsilon)
    end for
  }

  test("LU decomposition dimensions are correct") {
    // Test various matrix sizes
    val sizes = Seq((3, 3), (4, 3), (3, 4), (5, 5), (6, 4), (4, 6))

    for (rows, cols) <- sizes do
      val m = Matrix.rand(rows, cols)
      val luResult = lu(m)

      val minDim = math.min(rows, cols)
      assertEquals(luResult.L.rows, rows, s"L should have $rows rows for ${rows}x${cols} matrix")
      assertEquals(luResult.L.cols, minDim, s"L should have $minDim cols for ${rows}x${cols} matrix")
      assertEquals(luResult.U.rows, minDim, s"U should have $minDim rows for ${rows}x${cols} matrix")
      assertEquals(luResult.U.cols, cols, s"U should have $cols cols for ${rows}x${cols} matrix")
      assertEquals(luResult.P.length, minDim, s"P should have length $minDim for ${rows}x${cols} matrix")
    end for
  }

  test("LU decomposition with single element matrix") {
    val m = Matrix.zeros[Double](1, 1)
    m(0, 0) = 42.0

    val luResult = lu(m)

    assertEquals(luResult.L.rows, 1)
    assertEquals(luResult.L.cols, 1)
    assertEquals(luResult.U.rows, 1)
    assertEquals(luResult.U.cols, 1)

    assertEqualsDouble(luResult.L(0, 0), 1.0, epsilon)
    assertEqualsDouble(luResult.U(0, 0), 42.0, epsilon)
    assertEquals(luResult.P(0), 0)
  }

  test("LU decomposition is consistent across multiple calls") {
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(0, 2) = 3.0
    m(1, 0) = 4.0
    m(1, 1) = 5.0
    m(1, 2) = 6.0
    m(2, 0) = 7.0
    m(2, 1) = 8.0
    m(2, 2) = 9.0

    val luResult1 = lu(m)
    val luResult2 = lu(m)

    assert(matricesEqual(luResult1.L, luResult2.L), "L should be consistent across calls")
    assert(matricesEqual(luResult1.U, luResult2.U), "U should be consistent across calls")
    assert(luResult1.P.sameElements(luResult2.P), "P should be consistent across calls")
  }

  test("LU handles matrix with very small elements") {
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 1e-10
    m(0, 1) = 2e-10
    m(0, 2) = 3e-10
    m(1, 0) = 4e-10
    m(1, 1) = 5e-10
    m(1, 2) = 6e-10
    m(2, 0) = 7e-10
    m(2, 1) = 8e-10
    m(2, 2) = 9e-10

    val luResult = lu(m)

    assert(isLowerUnitTriangular(luResult.L), "L should be lower triangular with unit diagonal")
    assert(isUpperTriangular(luResult.U), "U should be upper triangular")

    // Verify P*A = L*U (with relaxed tolerance for small numbers)
    val PA = applyPermutation(m, luResult.P)
    val LU = luResult.L.matmul(luResult.U)(using false)
    assert(matricesEqual(PA, LU, 1e-15), "P*A should equal L*U")
  }

  test("LU correctly handles matrix with mixed positive and negative values") {
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 3.0
    m(0, 1) = -1.0
    m(0, 2) = 2.0
    m(1, 0) = -2.0
    m(1, 1) = 4.0
    m(1, 2) = -3.0
    m(2, 0) = 1.0
    m(2, 1) = -2.0
    m(2, 2) = 5.0

    val luResult = lu(m)

    assert(isLowerUnitTriangular(luResult.L), "L should be lower triangular with unit diagonal")
    assert(isUpperTriangular(luResult.U), "U should be upper triangular")

    // Verify P*A = L*U
    val PA = applyPermutation(m, luResult.P)
    val LU = luResult.L.matmul(luResult.U)(using false)
    assert(matricesEqual(PA, LU, 1e-9), "P*A should equal L*U")
  }

end LUSuite
