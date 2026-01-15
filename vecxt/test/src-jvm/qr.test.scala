package vecxt

import munit.FunSuite
import all.*
import BoundsCheck.DoBoundsCheck.yes

class QRSuite extends FunSuite:

  val epsilon = 1e-10

  // Helper to verify Q is orthogonal: Q^T * Q should equal identity
  def verifyOrthogonal(q: Matrix[Double], tol: Double = epsilon): Boolean =
    val qtq = q.transpose @@ q
    val n = q.rows
    var allMatch = true
    var i = 0
    while i < n && allMatch do
      var j = 0
      while j < n && allMatch do
        val expected = if i == j then 1.0 else 0.0
        val actual = qtq(i, j)
        if math.abs(actual - expected) > tol then allMatch = false
        end if
        j += 1
      end while
      i += 1
    end while
    allMatch
  end verifyOrthogonal

  // Helper to verify R is upper triangular
  def verifyUpperTriangular(r: Matrix[Double], tol: Double = epsilon): Boolean =
    val (m, n) = r.shape
    var allZero = true
    var i = 0
    while i < m && allZero do
      var j = 0
      while j < i && j < n && allZero do
        if math.abs(r(i, j)) > tol then allZero = false
        end if
        j += 1
      end while
      i += 1
    end while
    allZero
  end verifyUpperTriangular

  // Helper to verify A = Q * R
  def verifyDecomposition(a: Matrix[Double], q: Matrix[Double], r: Matrix[Double], tol: Double = epsilon): Boolean =
    val qr = q @@ r
    val (m, n) = a.shape
    var allMatch = true
    var i = 0
    while i < m && allMatch do
      var j = 0
      while j < n && allMatch do
        if math.abs(a(i, j) - qr(i, j)) > tol then allMatch = false
        end if
        j += 1
      end while
      i += 1
    end while
    allMatch
  end verifyDecomposition

  test("Identity matrix QR decomposition") {
    val id = Matrix.eye[Double](4)
    val (q, r) = qr(id)

    // Q should be identity (or a permutation of it)
    // R should be identity
    assert(verifyOrthogonal(q), "Q should be orthogonal")
    assert(verifyUpperTriangular(r), "R should be upper triangular")
    assert(verifyDecomposition(id, q, r), "A should equal Q*R")

    // For identity, R should also be identity
    for i <- 0 until 4 do assertEqualsDouble(r(i, i), 1.0, epsilon, "")
    end for
  }

  test("Diagonal matrix QR decomposition") {
    val diag = Matrix.zeros[Double](3, 3)
    diag(0, 0) = 2.0
    diag(1, 1) = 3.0
    diag(2, 2) = 5.0

    val (q, r) = qr(diag)

    assert(verifyOrthogonal(q), "Q should be orthogonal")
    assert(verifyUpperTriangular(r), "R should be upper triangular")
    assert(verifyDecomposition(diag, q, r), "A should equal Q*R")

    // Diagonal elements of R should match (possibly with sign changes)
    assertEqualsDouble(math.abs(r(0, 0)), 2.0, epsilon, "")
    assertEqualsDouble(math.abs(r(1, 1)), 3.0, epsilon, "")
    assertEqualsDouble(math.abs(r(2, 2)), 5.0, epsilon, "")
  }

  test("Simple 2x2 matrix QR decomposition") {
    val m = Matrix.zeros[Double](2, 2)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(1, 0) = 3.0
    m(1, 1) = 4.0

    val (q, r) = qr(m)

    assert(verifyOrthogonal(q), "Q should be orthogonal")
    assert(verifyUpperTriangular(r), "R should be upper triangular")
    assert(verifyDecomposition(m, q, r), "A should equal Q*R")
  }

  test("Tall matrix (more rows than columns)") {
    val m = Matrix.zeros[Double](4, 2)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(1, 0) = 3.0
    m(1, 1) = 4.0
    m(2, 0) = 5.0
    m(2, 1) = 6.0
    m(3, 0) = 7.0
    m(3, 1) = 8.0

    val (q, r) = qr(m)

    assert(verifyOrthogonal(q), "Q should be orthogonal")
    assert(verifyUpperTriangular(r), "R should be upper triangular")
    assert(verifyDecomposition(m, q, r), "A should equal Q*R")
    assertEquals(q.shape, (4, 4))
    assertEquals(r.shape, (4, 2))
  }

  test("Wide matrix (more columns than rows)") {
    val m = Matrix.zeros[Double](2, 4)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(0, 2) = 3.0
    m(0, 3) = 4.0
    m(1, 0) = 5.0
    m(1, 1) = 6.0
    m(1, 2) = 7.0
    m(1, 3) = 8.0

    val (q, r) = qr(m)

    assert(verifyOrthogonal(q), "Q should be orthogonal")
    assert(verifyUpperTriangular(r), "R should be upper triangular")
    assert(verifyDecomposition(m, q, r), "A should equal Q*R")
    assertEquals(q.shape, (2, 2))
    assertEquals(r.shape, (2, 4))
  }

  test("Square symmetric matrix") {
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 4.0
    m(0, 1) = 1.0
    m(0, 2) = 2.0
    m(1, 0) = 1.0
    m(1, 1) = 3.0
    m(1, 2) = 1.0
    m(2, 0) = 2.0
    m(2, 1) = 1.0
    m(2, 2) = 2.0

    val (q, r) = qr(m)

    assert(verifyOrthogonal(q), "Q should be orthogonal")
    assert(verifyUpperTriangular(r), "R should be upper triangular")
    assert(verifyDecomposition(m, q, r), "A should equal Q*R")
  }

  test("Upper triangular matrix remains upper triangular") {
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(0, 2) = 3.0
    m(1, 1) = 4.0
    m(1, 2) = 5.0
    m(2, 2) = 6.0

    val (q, r) = qr(m)

    assert(verifyOrthogonal(q), "Q should be orthogonal")
    assert(verifyUpperTriangular(r), "R should be upper triangular")
    assert(verifyDecomposition(m, q, r), "A should equal Q*R")
  }

  test("Matrix with a zero column") {
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 1.0
    m(0, 1) = 0.0
    m(0, 2) = 2.0
    m(1, 0) = 3.0
    m(1, 1) = 0.0
    m(1, 2) = 4.0
    m(2, 0) = 5.0
    m(2, 1) = 0.0
    m(2, 2) = 6.0

    val (q, r) = qr(m)

    assert(verifyOrthogonal(q), "Q should be orthogonal")
    assert(verifyUpperTriangular(r), "R should be upper triangular")
    assert(verifyDecomposition(m, q, r), "A should equal Q*R")

    // The second column of R should be zero
    assertEqualsDouble(r(0, 1), 0.0, epsilon, "")
    assertEqualsDouble(r(1, 1), 0.0, epsilon, "")
  }

  test("Single column matrix") {
    val m = Matrix.zeros[Double](3, 1)
    m(0, 0) = 1.0
    m(1, 0) = 2.0
    m(2, 0) = 3.0

    val (q, r) = qr(m)

    assert(verifyOrthogonal(q), "Q should be orthogonal")
    assert(verifyUpperTriangular(r), "R should be upper triangular")
    assert(verifyDecomposition(m, q, r), "A should equal Q*R")

    // R should have only one element in the first position
    val expectedNorm = math.sqrt(1.0 * 1.0 + 2.0 * 2.0 + 3.0 * 3.0)
    assertEqualsDouble(math.abs(r(0, 0)), expectedNorm, epsilon, "")
  }

  test("Single row matrix") {
    val m = Matrix.zeros[Double](1, 3)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(0, 2) = 3.0

    val (q, r) = qr(m)

    assert(verifyOrthogonal(q), "Q should be orthogonal")
    assert(verifyUpperTriangular(r), "R should be upper triangular")
    assert(verifyDecomposition(m, q, r), "A should equal Q*R")

    // Q should be [1] or [-1] for a 1x1 matrix
    assertEqualsDouble(math.abs(q(0, 0)), 1.0, epsilon, "")
  }

  test("Single element matrix") {
    val m = Matrix.zeros[Double](1, 1)
    m(0, 0) = 42.0

    val (q, r) = qr(m)

    assert(verifyOrthogonal(q), "Q should be orthogonal")
    assert(verifyUpperTriangular(r), "R should be upper triangular")
    assert(verifyDecomposition(m, q, r), "A should equal Q*R")

    assertEqualsDouble(math.abs(q(0, 0)), 1.0, epsilon, "")
    assertEqualsDouble(math.abs(r(0, 0)), 42.0, epsilon, "")
  }

  test("QR decomposition preserves original matrix") {
    val original = Matrix.zeros[Double](3, 3)
    original(0, 0) = 1.0
    original(0, 1) = 2.0
    original(0, 2) = 3.0
    original(1, 0) = 4.0
    original(1, 1) = 5.0
    original(1, 2) = 6.0
    original(2, 0) = 7.0
    original(2, 1) = 8.0
    original(2, 2) = 9.0

    val copy = original.deepCopy

    val (q, r) = qr(original)

    // Verify original is unchanged
    for i <- 0 until 3; j <- 0 until 3 do assertEqualsDouble(original(i, j), copy(i, j), epsilon, "")
    end for
  }

  test("Random matrices") {
    val rand = scala.util.Random(42)
    val size = 5
    for _ <- 0 until 10 do
      val m = Matrix.zeros[Double](size, size)
      for i <- 0 until size; j <- 0 until size do m(i, j) = rand.nextDouble() * 10 - 5
      end for

      val (q, r) = qr(m)

      assert(verifyOrthogonal(q), "Q should be orthogonal")
      assert(verifyUpperTriangular(r), "R should be upper triangular")
      assert(verifyDecomposition(m, q, r), "A should equal Q*R")
    end for
  }

  test("Random tall matrices") {
    val rand = scala.util.Random(123)
    val rows = 6
    val cols = 4
    for _ <- 0 until 5 do
      val m = Matrix.zeros[Double](rows, cols)
      for i <- 0 until rows; j <- 0 until cols do m(i, j) = rand.nextDouble() * 10 - 5
      end for

      val (q, r) = qr(m)

      assert(verifyOrthogonal(q), "Q should be orthogonal")
      assert(verifyUpperTriangular(r), "R should be upper triangular")
      assert(verifyDecomposition(m, q, r), "A should equal Q*R")
    end for
  }

  test("Random wide matrices") {
    val rand = scala.util.Random(456)
    val rows = 3
    val cols = 5
    for _ <- 0 until 5 do
      val m = Matrix.zeros[Double](rows, cols)
      for i <- 0 until rows; j <- 0 until cols do m(i, j) = rand.nextDouble() * 10 - 5
      end for

      val (q, r) = qr(m)

      assert(verifyOrthogonal(q), "Q should be orthogonal")
      assert(verifyUpperTriangular(r), "R should be upper triangular")
      assert(verifyDecomposition(m, q, r), "A should equal Q*R")
    end for
  }

  test("QR determinant property for square matrices") {
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

    val (q, r) = qr(m)

    // For square matrices: det(A) = det(Q) * det(R)
    // det(Q) = ±1 (orthogonal matrix)
    // det(R) = product of diagonal elements (upper triangular)
    val detA = m.det
    val detR = (0 until 3).map(i => r(i, i)).product

    // |det(A)| should equal |det(R)| since |det(Q)| = 1
    assertEqualsDouble(math.abs(detA), math.abs(detR), epsilon, "")
  }

  test("Applying QR to solve least squares (overdetermined system)") {
    // Create a simple overdetermined system: 4 equations, 2 unknowns
    // A*x = b
    val a = Matrix.zeros[Double](4, 2)
    a(0, 0) = 1.0
    a(0, 1) = 1.0
    a(1, 0) = 1.0
    a(1, 1) = 2.0
    a(2, 0) = 1.0
    a(2, 1) = 3.0
    a(3, 0) = 1.0
    a(3, 1) = 4.0

    val b = Array(2.0, 3.0, 5.0, 7.0)

    // QR decomposition
    val (q, r) = qr(a)

    // Solve R*x = Q^T*b for the upper 2x2 part of R
    val qt = q.transpose
    val qtb = Array.ofDim[Double](4)
    for i <- 0 until 4 do qtb(i) = (0 until 4).map(k => qt(i, k) * b(k)).sum
    end for

    // Back substitution on the first 2 rows of R
    val x = Array.ofDim[Double](2)
    x(1) = qtb(1) / r(1, 1)
    x(0) = (qtb(0) - r(0, 1) * x(1)) / r(0, 0)

    // The solution should minimize ||A*x - b||
    // For this specific problem, we can verify the solution makes sense
    // Not exact due to overdetermined system, but should be reasonable
    val residual = Array.ofDim[Double](4)
    for i <- 0 until 4 do residual(i) = b(i) - (a(i, 0) * x(0) + a(i, 1) * x(1))
    end for

    val residualNorm = math.sqrt(residual.map(r => r * r).sum)

    // Residual should be relatively small (not zero due to overdetermined system)
    assert(residualNorm < 1.0, s"Residual norm should be small, got $residualNorm")
  }

  test("Large matrix QR decomposition") {
    val size = 20
    val m = Matrix.zeros[Double](size, size)
    val rand = scala.util.Random(789)
    for i <- 0 until size; j <- 0 until size do m(i, j) = rand.nextDouble() * 10 - 5
    end for

    val (q, r) = qr(m)

    assert(verifyOrthogonal(q, 1e-9), "Q should be orthogonal")
    assert(verifyUpperTriangular(r, 1e-9), "R should be upper triangular")
    assert(verifyDecomposition(m, q, r, 1e-8), "A should equal Q*R")
  }

  test("QR with rank-deficient matrix") {
    // Create a rank-deficient matrix (rank 2, size 3x3)
    val m = Matrix.zeros[Double](3, 3)
    m(0, 0) = 1.0
    m(0, 1) = 2.0
    m(0, 2) = 3.0
    m(1, 0) = 2.0
    m(1, 1) = 4.0
    m(1, 2) = 6.0 // Second row is 2x first row
    m(2, 0) = 3.0
    m(2, 1) = 6.0
    m(2, 2) = 9.0 // Third row is 3x first row

    val (q, r) = qr(m)

    assert(verifyOrthogonal(q), "Q should be orthogonal")
    assert(verifyUpperTriangular(r), "R should be upper triangular")
    assert(verifyDecomposition(m, q, r), "A should equal Q*R")

    // The third row of R should be (nearly) zero since the matrix is rank-deficient
    assertEqualsDouble(r(2, 0), 0.0, 1e-9, "")
    assertEqualsDouble(r(2, 1), 0.0, 1e-9, "")
    assertEqualsDouble(r(2, 2), 0.0, 1e-9, "")
  }

end QRSuite
