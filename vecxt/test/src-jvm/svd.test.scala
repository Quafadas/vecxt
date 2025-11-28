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
import scala.util.Random
class SvdSuite extends FunSuite:

  val epsilon = 1e-10

  def assertMatrixEquals(actual: Matrix[Double], expected: Matrix[Double], tol: Double = epsilon): Unit =
    assertEquals(actual.shape, expected.shape, "Matrix dimensions don't match")
    val (m, n) = actual.shape
    for
      i <- 0 until m
      j <- 0 until n
    do assertEqualsDouble(actual(i, j), expected(i, j), tol, s"Element at ($i, $j) differs")
    end for
  end assertMatrixEquals

  def assertArrayEquals(actual: Array[Double], expected: Array[Double], tol: Double = epsilon): Unit =
    assertEquals(actual.length, expected.length, "Array lengths don't match")
    actual.zip(expected).zipWithIndex.foreach { case ((a, e), i) =>
      assertEqualsDouble(a, e, tol, s"Element at index $i differs")
    }
  end assertArrayEquals

  def reconstruct(U: Matrix[Double], s: Array[Double], Vt: Matrix[Double]): Matrix[Double] =
    val k = s.length
    val S = Matrix.zeros[Double](k, k)
    var i = 0
    while i < k do
      S(i, i) = s(i)
      i += 1
    end while
    U.matmul(S).matmul(Vt)
  end reconstruct

  def relativeFrobeniusError(actual: Matrix[Double], expected: Matrix[Double]): Double =
    assertEquals(actual.shape, expected.shape, "Matrix dimensions must match for error computation")
    val (m, n) = actual.shape
    var diffSq = 0.0
    var refSq = 0.0
    var row = 0
    while row < m do
      var col = 0
      while col < n do
        val d = actual(row, col) - expected(row, col)
        diffSq += d * d
        val ref = expected(row, col)
        refSq += ref * ref
        col += 1
      end while
      row += 1
    end while
    val denom = math.max(math.sqrt(refSq), 1e-30)
    math.sqrt(diffSq) / denom
  end relativeFrobeniusError

  def assertOrthonormalCols(Q: Matrix[Double], tol: Double = epsilon): Unit =
    val (_, n) = Q.shape
    val gram = Q.T.matmul(Q)
    val I = Matrix.eye[Double](n)
    assertMatrixEquals(gram, I, tol)
  end assertOrthonormalCols

  test("SVD CompleteSVD - square matrix reconstruction"):
    // Create a simple 3x3 matrix
    val A = Matrix(
      Array[Double](
        1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0
      ),
      3,
      3
    )

    val result = svd(A, SVDMode.CompleteSVD)

    // Check dimensions
    assertEquals(result.U.shape, (3, 3), "U should be 3x3 for CompleteSVD")
    assertEquals(result.s.length, 3, "s should have 3 singular values")
    assertEquals(result.Vt.shape, (3, 3), "Vt should be 3x3 for CompleteSVD")

    // Check singular values are non-negative and sorted in descending order
    assert(result.s.forall(_ >= 0.0), "All singular values should be non-negative")
    assert(
      result.s.zip(result.s.tail).forall { case (a, b) => a >= b },
      "Singular values should be in descending order"
    )

  test("SVD ReducedSVD - square matrix reconstruction"):
    val A = Matrix(
      Array[Double](
        1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0
      ),
      3,
      3
    )

    val result = svd(A, SVDMode.ReducedSVD)

    // Check dimensions for reduced SVD (same as complete for square matrix)
    assertEquals(result.U.shape, (3, 3), "U should be 3x3 for ReducedSVD on square matrix")
    assertEquals(result.s.length, 3, "s should have 3 singular values")
    assertEquals(result.Vt.shape, (3, 3), "Vt should be 3x3 for ReducedSVD on square matrix")

  test("SVD CompleteSVD - tall matrix (m > n)"):
    // Create a 4x2 matrix
    val A = Matrix(
      Array[Double](
        1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0
      ),
      4,
      2
    )

    val result = svd(A, SVDMode.CompleteSVD)

    // Check dimensions
    assertEquals(result.U.shape, (4, 4), "U should be 4x4 for CompleteSVD")
    assertEquals(result.s.length, 2, "s should have min(4,2)=2 singular values")
    assertEquals(result.Vt.shape, (2, 2), "Vt should be 2x2 for CompleteSVD")

    // Verify singular values properties
    assert(result.s.forall(_ >= 0.0), "All singular values should be non-negative")
    assert(
      result.s.zip(result.s.tail).forall { case (a, b) => a >= b },
      "Singular values should be in descending order"
    )

  test("SVD ReducedSVD - tall matrix (m > n)"):
    val A = Matrix(
      Array[Double](
        1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0
      ),
      4,
      2
    )

    val result = svd(A, SVDMode.ReducedSVD)

    // Check dimensions for reduced SVD
    assertEquals(result.U.shape, (4, 2), "U should be 4x2 for ReducedSVD (m x min(m,n))")
    assertEquals(result.s.length, 2, "s should have 2 singular values")
    assertEquals(result.Vt.shape, (2, 2), "Vt should be 2x2 for ReducedSVD")

  test("SVD CompleteSVD - wide matrix (m < n)"):
    // Create a 2x4 matrix
    val A = Matrix(
      Array[Double](
        1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0
      ),
      2,
      4
    )

    val result = svd(A, SVDMode.CompleteSVD)

    // Check dimensions
    assertEquals(result.U.shape, (2, 2), "U should be 2x2 for CompleteSVD")
    assertEquals(result.s.length, 2, "s should have min(2,4)=2 singular values")
    assertEquals(result.Vt.shape, (4, 4), "Vt should be 4x4 for CompleteSVD")

  test("SVD ReducedSVD - wide matrix (m < n)"):
    val A = Matrix(
      Array[Double](
        1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0
      ),
      2,
      4
    )

    val result = svd(A, SVDMode.ReducedSVD)

    // Check dimensions for reduced SVD
    assertEquals(result.U.shape, (2, 2), "U should be 2x2 for ReducedSVD")
    assertEquals(result.s.length, 2, "s should have 2 singular values")
    assertEquals(result.Vt.shape, (2, 4), "Vt should be 2x4 for ReducedSVD (min(m,n) x n)")

  test("SVD - identity matrix"):
    val I = Matrix(
      Array[Double](
        1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0
      ),
      3,
      3
    )

    val result = svd(I, SVDMode.CompleteSVD)

    // Singular values should all be 1.0
    assertArrayEquals(result.s, Array(1.0, 1.0, 1.0), 1e-10)

  test("SVD - diagonal matrix"):
    val D = Matrix(
      Array[Double](
        5.0, 0.0, 0.0, 0.0, 3.0, 0.0, 0.0, 0.0, 1.0
      ),
      3,
      3
    )

    val result = svd(D, SVDMode.CompleteSVD)

    // Singular values should be the absolute values of diagonal elements in descending order
    assertArrayEquals(result.s, Array(5.0, 3.0, 1.0), 1e-10)

  test("SVD - reconstruction A = U * S * Vt (ReducedSVD)"):
    val A = Matrix(
      Array[Double](
        2.0, 4.0, 1.0, 3.0, 0.0, 0.0, 0.0, 0.0
      ),
      4,
      2
    )

    val result = svd(A, SVDMode.ReducedSVD)

    // Create diagonal matrix S from singular values
    val S = Matrix.zeros[Double](2, 2)
    for i <- 0 until 2 do S(i, i) = result.s(i)
    end for

    // Reconstruct: A_reconstructed = U * S * Vt
    val US = result.U.matmul(S)
    val reconstructed = US.matmul(result.Vt)

    assertMatrixEquals(reconstructed, A, 1e-10)

  test("SVD - orthogonality of U (CompleteSVD)"):
    val A = Matrix(
      Array[Double](
        1.0, 2.0, 3.0, 4.0, 5.0, 6.0
      ),
      3,
      2
    )

    val result = svd(A, SVDMode.CompleteSVD)

    // U^T * U should be identity
    val UtU = result.U.T.matmul(result.U)
    val I = Matrix.eye[Double](3)

    assertMatrixEquals(UtU, I, 1e-10)

  test("SVD - orthogonality of Vt (CompleteSVD)"):
    val A = Matrix(
      Array[Double](
        1.0, 2.0, 3.0, 4.0, 5.0, 6.0
      ),
      3,
      2
    )

    val result = svd(A, SVDMode.CompleteSVD)

    // Vt * Vt^T should be identity
    val VtVtt = result.Vt.matmul(result.Vt.T)
    val I = Matrix.eye[Double](2)

    assertMatrixEquals(VtVtt, I, 1e-10)

  test("SVD - rank deficient matrix"):
    // Create a rank-1 matrix (all rows are multiples of first row)
    val A = Matrix(
      Array[Double](
        1.0, 2.0, 3.0, 2.0, 4.0, 6.0, 3.0, 6.0, 9.0
      ),
      3,
      3
    )

    val result = svd(A, SVDMode.CompleteSVD)

    // Should have only 1 non-zero singular value (within numerical precision)
    val nonZeroCount = result.s.count(_ > 1e-10)
    assertEquals(nonZeroCount, 1, "Rank-1 matrix should have 1 non-zero singular value")

  test("SVD - zero matrix"):
    val zeros = Matrix.zeros[Double](3, 3)

    val result = svd(zeros, SVDMode.CompleteSVD)

    // All singular values should be zero
    assert(result.s.forall(math.abs(_) < 1e-10), "All singular values should be zero for zero matrix")

  test("SVD - single element matrix"):
    val A = Matrix(Array[Double](5.0), 1, 1)

    val result = svd(A, SVDMode.CompleteSVD)

    assertEquals(result.U.shape, (1, 1))
    assertEquals(result.s.length, 1)
    assertEquals(result.Vt.shape, (1, 1))
    assertEqualsDouble(result.s(0), 5.0, epsilon)

  test("SVD - negative values"):
    val A = Matrix(
      Array[Double](
        -1.0,
        -2.0,
        -3.0,
        -4.0
      ),
      2,
      2
    )

    val result = svd(A, SVDMode.CompleteSVD)

    // Singular values should still be non-negative
    assert(result.s.forall(_ >= 0.0), "Singular values should be non-negative even for negative input")

    // Reconstruction should work
    val S = Matrix.zeros[Double](2, 2)
    for i <- 0 until 2 do S(i, i) = result.s(i)
    end for

    val reconstructed = result.U.matmul(S).matmul(result.Vt)
    assertMatrixEquals(reconstructed, A, 1e-10)

  test("SVD - does not mutate input matrix"):
    val original = Matrix(
      Array[Double](
        1.0,
        2.0,
        3.0,
        4.0
      ),
      2,
      2
    )

    val originalCopy = original.deepCopy

    val result = svd(original, SVDMode.CompleteSVD)

    // Original matrix should be unchanged
    assertMatrixEquals(original, originalCopy, epsilon)

  test("SVD - random rectangular matrices maintain reconstruction accuracy"):
    val rng = new Random(42)
    val rows = 6
    val cols = 4
    val data = Array.fill(rows * cols)(rng.nextGaussian())
    val A = Matrix(data, rows, cols)

    val result = svd(A, SVDMode.ReducedSVD)

    val reconstructed = reconstruct(result.U, result.s, result.Vt)
    val relErr = relativeFrobeniusError(reconstructed, A)
    assert(relErr < 1e-10, s"Reconstruction relative error $relErr was too large")
    assertOrthonormalCols(result.U)
    assertOrthonormalCols(result.Vt.T)

  test("SVD - handles extremely scaled magnitudes"):
    val scale = 1e150
    val A = Matrix(
      Array[Double](
        scale,
        0.0,
        0.0,
        0.0,
        scale * 1e-120,
        0.0,
        0.0,
        0.0,
        1e-150
      ),
      3,
      3
    )

    val result = svd(A, SVDMode.CompleteSVD)

    val expected = Array(scale, scale * 1e-120, 1e-150)
    expected.zip(result.s).foreach { case (exp, got) =>
      val rel = math.abs(got - exp) / math.max(math.abs(exp), 1e-300)
      assert(rel < 1e-10, s"Singular value $got differed from expected $exp with relative error $rel")
    }

  test("SVD - near rank deficient matrix reveals tiny singular values"):
    // Matrix is column-major, so data is arranged by columns
    val A = Matrix(
      Array[Double](
        1.0,
        2.0,
        3.0,
        4.0, // Column 0
        2.0,
        4.0,
        6.0,
        8.0 - 1e-8, // Column 1
        3.0,
        6.0,
        9.0 + 1e-8,
        12.0 // Column 2
      ),
      4,
      3
    )

    val result = svd(A, SVDMode.ReducedSVD)

    assert(result.s(0) > 1.0)
    assert(result.s(1) < 1e-6, s"Second singular value ${result.s(1)} should reflect near rank deficiency")
    assert(result.s(2) < 1e-8, s"Third singular value ${result.s(2)} should be tiny")

  test("SVD - matches golden singular values for 3x2 matrix"):
    // Matrix is column-major, so data is arranged by columns
    val A = Matrix(
      Array[Double](
        1.0, 3.0, -1.0, // Column 0
        2.0, 4.0, 0.5 // Column 1
      ),
      3,
      2
    )

    val result = svd(A, SVDMode.ReducedSVD)

    val expected = Array(5.467656780575479, 1.1639284040811924)
    expected.zip(result.s).foreach { case (exp, got) =>
      assertEqualsDouble(got, exp, 1e-12, s"Singular value mismatch for golden matrix")
    }

  test("SVD - multiple random trials maintain orthogonality"):
    val rng = new Random(7)
    val trials = 5
    var t = 0
    while t < trials do
      val rows = 5
      val cols = 5
      val data = Array.fill(rows * cols)(rng.nextDouble() * 2 - 1)
      val A = Matrix(data, rows, cols)
      val result = svd(A, SVDMode.CompleteSVD)

      assertOrthonormalCols(result.U)
      assertOrthonormalCols(result.Vt.T)
      t += 1
    end while

  test("pinv - matches inverse for well-conditioned square matrix"):
    val A = Matrix(
      Array[Double](
        4.0,
        7.0,
        2.0,
        6.0
      ),
      2,
      2
    )

    val pseudoInv = pinv(A)
    val eye2 = Matrix.eye[Double](2)

    assertMatrixEquals(A.matmul(pseudoInv), eye2, 1e-10)
    assertMatrixEquals(pseudoInv.matmul(A), eye2, 1e-10)

  test("pinv - yields left inverse for tall full-rank matrix"):
    val A = Matrix(
      Array[Double](
        1.0, 4.0, 2.0, 5.0, 3.0, 6.0
      ),
      3,
      2
    )

    val pseudoInv = pinv(A)
    val eye2 = Matrix.eye[Double](2)

    val leftIdentity = pseudoInv.matmul(A)
    assertMatrixEquals(leftIdentity, eye2, 1e-10)

    val projection = A.matmul(pseudoInv)
    assertMatrixEquals(projection.matmul(A), A, 1e-10)

  test("pinv - preserves A*A+*A for rank-deficient matrix"):
    val A = Matrix(
      Array[Double](
        1.0,
        2.0,
        2.0,
        4.0
      ),
      2,
      2
    )

    val pseudoInv = pinv(A)
    val reconstructed = A.matmul(pseudoInv).matmul(A)

    assertMatrixEquals(reconstructed, A, 1e-10)

end SvdSuite
