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

class EigenValueSuite extends FunSuite:

  val epsilon = 1e-10

  // Helper to check if all eigenvalues are real (no complex parts)
  // IMPORTANT: When eigenvalues have complex parts, the realParts and imagParts arrays
  // form conjugate pairs and must stay aligned. Sorting realParts alone would break
  // the correspondence with imagParts, making the results meaningless.
  // Only sort eigenvalues when areAllReal returns true.
  def areAllReal(complexParts: Array[Double], tol: Double = epsilon): Boolean =
    complexParts.forall(c => math.abs(c) < tol)

  test("Identity matrix eigenvalues should be 1.0") {
    val id = Matrix.eye[Double](4)
    val (eigenvalues, complexEigenValues, eigenVectors) = eig(id)

    // All eigenvalues should be 1.0
    eigenvalues.foreach(ev => assertEqualsDouble(ev, 1.0, epsilon))

    // No complex parts
    complexEigenValues.foreach(ev => assertEqualsDouble(ev, 0.0, epsilon))
  }

  test("Diagonal matrix eigenvalues should equal diagonal entries") {
    val diag = Matrix.zeros[Double](3, 3)
    diag(0, 0) = 2.0
    diag(1, 1) = 3.0
    diag(2, 2) = 5.0

    val (eigenvalues, complexEigenValues, _) = eig(diag)

    // No complex parts for diagonal matrix
    assert(areAllReal(complexEigenValues), "Diagonal matrix should have only real eigenvalues")

    // Sort eigenvalues for comparison (safe since all are real)
    val sortedEigs = eigenvalues.sorted

    assertEqualsDouble(sortedEigs(0), 2.0, epsilon)
    assertEqualsDouble(sortedEigs(1), 3.0, epsilon)
    assertEqualsDouble(sortedEigs(2), 5.0, epsilon)
  }

  test("2x2 symmetric matrix eigenvalues") {
    // [4.0, 1.0]
    // [1.0, 3.0]
    // Known eigenvalues: (7 + sqrt(5))/2 ≈ 4.618 and (7 - sqrt(5))/2 ≈ 2.382
    val m = Matrix.zeros[Double](2, 2)
    m(0, 0) = 4.0
    m(0, 1) = 1.0
    m(1, 0) = 1.0
    m(1, 1) = 3.0

    val (eigenvalues, complexEigenValues, _) = eig(m)

    // Symmetric matrices have real eigenvalues
    assert(areAllReal(complexEigenValues), "Symmetric matrix should have only real eigenvalues")

    val expected1 = (7.0 + math.sqrt(5.0)) / 2.0
    val expected2 = (7.0 - math.sqrt(5.0)) / 2.0

    // Safe to sort since all eigenvalues are real
    val sortedEigs = eigenvalues.sorted

    assertEqualsDouble(sortedEigs(0), expected2, epsilon)
    assertEqualsDouble(sortedEigs(1), expected1, epsilon)
  }

  test("3x3 symmetric matrix with known eigenvalues") {
    // [6.0, 2.0, 1.0]
    // [2.0, 3.0, 1.0]
    // [1.0, 1.0, 1.0]
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 6.0
    m(0, 1) = 2.0
    m(0, 2) = 1.0
    m(1, 0) = 2.0
    m(1, 1) = 3.0
    m(1, 2) = 1.0
    m(2, 0) = 1.0
    m(2, 1) = 1.0
    m(2, 2) = 1.0

    val (eigenvalues, complexEigenValues, eigenVectors) = eig(m)

    // Verify that eigenvalues are real (no complex components)
    complexEigenValues.foreach(ev => assertEqualsDouble(ev, 0.0, epsilon))

    // Trace should equal sum of eigenvalues
    val trace = m(0, 0) + m(1, 1) + m(2, 2)
    val eigSum = eigenvalues.sum
    assertEqualsDouble(eigSum, trace, epsilon)
  }

  test("Eigenvalue decomposition satisfies A*v = λ*v") {
    // Create a simple symmetric matrix
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 4.0
    m(0, 1) = 1.0
    m(0, 2) = 0.0
    m(1, 0) = 1.0
    m(1, 1) = 3.0
    m(1, 2) = 1.0
    m(2, 0) = 0.0
    m(2, 1) = 1.0
    m(2, 2) = 2.0

    val (eigenvalues, _, eigenVectors) = eig(m)

    // For each eigenvalue and corresponding eigenvector, verify A*v = λ*v
    for i <- 0 until eigenvalues.length do
      val lambda = eigenvalues(i)
      val v = Array.ofDim[Double](3)
      // Extract i-th column of eigenVectors (column-major layout)
      for j <- 0 until 3 do v(j) = eigenVectors(j, i)
      end for

      // Compute A*v
      val av = Array.ofDim[Double](3)
      for row <- 0 until 3 do av(row) = (0 until 3).map(col => m(row, col) * v(col)).sum
      end for

      // Compute λ*v
      val lambdav = v.map(_ * lambda)

      // Verify A*v ≈ λ*v
      for j <- 0 until 3 do assertEqualsDouble(av(j), lambdav(j), 1e-8)
      end for
    end for
  }

  test("Zero matrix has all zero eigenvalues") {
    val m = Matrix.zeros[Double](3, 3)
    val (eigenvalues, complexEigenValues, _) = eig(m)

    eigenvalues.foreach(ev => assertEqualsDouble(ev, 0.0, epsilon))
    complexEigenValues.foreach(ev => assertEqualsDouble(ev, 0.0, epsilon))
  }

  test("Scaled identity matrix eigenvalues") {
    val scale = 7.5
    val m = Matrix.eye[Double](4)
    for i <- 0 until 4 do m(i, i) = scale
    end for

    val (eigenvalues, _, _) = eig(m)
    eigenvalues.foreach(ev => assertEqualsDouble(ev, scale, epsilon))
  }

  test("Upper triangular matrix eigenvalues equal diagonal") {
    // Upper triangular matrices have eigenvalues on the diagonal
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(0, 2) = 3.0
    m(1, 1) = 4.0
    m(1, 2) = 5.0
    m(2, 2) = 6.0

    val (eigenvalues, complexEigenValues, _) = eig(m)

    // Triangular matrices have real eigenvalues (on the diagonal)
    assert(areAllReal(complexEigenValues), "Triangular matrix should have only real eigenvalues")

    val sortedEigs = eigenvalues.sorted

    assertEqualsDouble(sortedEigs(0), 1.0, epsilon)
    assertEqualsDouble(sortedEigs(1), 4.0, epsilon)
    assertEqualsDouble(sortedEigs(2), 6.0, epsilon)
  }

  test("Lower triangular matrix eigenvalues equal diagonal") {
    // Lower triangular matrices have eigenvalues on the diagonal
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 2.0
    m(1, 0) = 3.0
    m(1, 1) = 5.0
    m(2, 0) = 4.0
    m(2, 1) = 6.0
    m(2, 2) = 7.0

    val (eigenvalues, complexEigenValues, _) = eig(m)

    // Triangular matrices have real eigenvalues (on the diagonal)
    assert(areAllReal(complexEigenValues), "Triangular matrix should have only real eigenvalues")

    val sortedEigs = eigenvalues.sorted

    assertEqualsDouble(sortedEigs(0), 2.0, epsilon)
    assertEqualsDouble(sortedEigs(1), 5.0, epsilon)
    assertEqualsDouble(sortedEigs(2), 7.0, epsilon)
  }

  test("Rotation matrix has complex eigenvalues") {
    // 90-degree rotation matrix in 2D
    val m = Matrix.zeros[Double](2, 2)
    m(0, 0) = 0.0
    m(0, 1) = -1.0
    m(1, 0) = 1.0
    m(1, 1) = 0.0

    val (eigenvalues, complexEigenValues, _) = eig(m)

    // For a rotation matrix, we expect complex eigenvalues with magnitude 1
    // The real parts should be ~0 and imaginary parts should be ±1
    val mags = eigenvalues.zip(complexEigenValues).map((r, i) => math.sqrt(r * r + i * i))
    mags.foreach(m => assertEqualsDouble(m, 1.0, epsilon))

    // Complex parts should be ±1
    assertEqualsDouble(math.abs(complexEigenValues(0)), 1.0, epsilon)
    assertEqualsDouble(math.abs(complexEigenValues(1)), 1.0, epsilon)
  }

  test("Eigenvalues of transpose equal eigenvalues of original") {
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(0, 2) = 0.0
    m(1, 0) = 0.0
    m(1, 1) = 3.0
    m(1, 2) = 1.0
    m(2, 0) = 1.0
    m(2, 1) = 0.0
    m(2, 2) = 2.0

    val (eigenvalues1, complexEigenValues1, _) = eig(m)
    val mT = vecxt.matrixUtil.transpose(m)
    val (eigenvalues2, complexEigenValues2, _) = eig(mT)

    // Only sort if all eigenvalues are real for both matrices
    if areAllReal(complexEigenValues1) && areAllReal(complexEigenValues2) then
      val sorted1 = eigenvalues1.sorted
      val sorted2 = eigenvalues2.sorted
      for i <- 0 until sorted1.length do assertEqualsDouble(sorted1(i), sorted2(i), epsilon)
      end for
    else
      // For complex eigenvalues, we need to compare magnitudes or use a more sophisticated matching
      // For this test, we'll just verify the trace equality (sum of eigenvalues is invariant)
      assertEqualsDouble(eigenvalues1.sum, eigenvalues2.sum, epsilon)
    end if
  }

  test("Single element matrix") {
    val m = Matrix.zeros[Double](1, 1)
    m(0, 0) = 42.0

    val (eigenvalues, complexEigenValues, eigenVectors) = eig(m)

    assertEqualsDouble(eigenvalues(0), 42.0, epsilon)
    assertEqualsDouble(complexEigenValues(0), 0.0, epsilon)
    assertEqualsDouble(eigenVectors(0, 0), 1.0, epsilon)
  }

  test("Should throw on NaN input when bounds checking enabled") {
    val m = Matrix.zeros[Double](2, 2)
    m(0, 0) = Double.NaN
    m(1, 1) = 1.0

    intercept[IllegalArgumentException] {
      eig(m)(using yes)
    }
  }

  test("Determinant equals product of eigenvalues") {
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 2.0
    m(0, 1) = 1.0
    m(0, 2) = 0.0
    m(1, 0) = 1.0
    m(1, 1) = 3.0
    m(1, 2) = 1.0
    m(2, 0) = 0.0
    m(2, 1) = 1.0
    m(2, 2) = 2.0

    val (eigenvalues, _, _) = eig(m)
    val eigProduct = eigenvalues.product
    val det = m.det

    assertEqualsDouble(eigProduct, det, epsilon)
  }

  test("Non-symmetric matrix with real eigenvalues") {
    // Matrix:
    // [1 4]
    // [0 2]
    //
    // This matrix is not symmetric, but has real eigenvalues 1 and 2.
    val m = Matrix.zeros[Double](2, 2)
    m(0, 0) = 1.0
    m(0, 1) = 4.0
    m(1, 0) = 0.0
    m(1, 1) = 2.0

    val (eigenvalues, complexEigenValues, eigenVectors) = eig(m)

    // All eigenvalues should be real
    assert(areAllReal(complexEigenValues), "This upper triangular matrix should have only real eigenvalues")

    // Sort for consistent comparison (safe since all are real)
    val sorted = eigenvalues.sorted

    assertEqualsDouble(sorted(0), 1.0, epsilon)
    assertEqualsDouble(sorted(1), 2.0, epsilon)
  }

  test("Sum of eigenvalues equals trace") {
    val m = Matrix.zeros[Double](4, 4)
    m(0, 0) = 5.0
    m(1, 1) = 3.0
    m(2, 2) = 7.0
    m(3, 3) = 2.0
    m(0, 1) = 1.0
    m(1, 2) = 0.5
    m(2, 3) = 0.3

    val (eigenvalues, _, _) = eig(m)
    val trace = (0 until 4).map(i => m(i, i)).sum

    assertEqualsDouble(eigenvalues.sum, trace, epsilon)
  }

  test("Random symmetric matrices have real eigenvalues") {
    val rand = scala.util.Random(0)
    for _ <- 0 until 50 do
      val m = Matrix.zeros[Double](5, 5)
      for i <- 0 until 5; j <- i until 5 do
        val v = rand.nextDouble() * 2 - 1
        m(i, j) = v
        m(j, i) = v
      end for
      val (_, complexEigenValues, _) = eig(m)
      assert(areAllReal(complexEigenValues, 1e-8), "Symmetric matrices should have only real eigenvalues")
    end for
  }

  test("Complex conjugate pairs must stay aligned - sorting breaks correspondence") {
    // Matrix with known complex eigenvalues: i and -i
    // [0  1]
    // [-1 0]
    val m = Matrix.zeros[Double](2, 2)
    m(0, 0) = 0.0
    m(0, 1) = 1.0
    m(1, 0) = -1.0
    m(1, 1) = 0.0

    val (realParts, imagParts, _) = eig(m)

    // Both eigenvalues should be purely imaginary: 0 ± i
    realParts.foreach(r => assertEqualsDouble(r, 0.0, epsilon))

    // Verify we have complex conjugate pairs
    assert(!areAllReal(imagParts), "This matrix should have complex eigenvalues")

    // The key point: sorting realParts would not change order (both are ~0),
    // but if realParts were different, sorting them would break the pairing with imagParts
    // This test documents that eigenvalue arrays must be kept in sync
    for i <- 0 until realParts.length do
      val magnitude = math.sqrt(realParts(i) * realParts(i) + imagParts(i) * imagParts(i))
      assertEqualsDouble(magnitude, 1.0, epsilon)
    end for
  }

end EigenValueSuite
