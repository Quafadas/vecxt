package vecxt

import dev.ludovic.netlib.lapack.JavaLAPACK
import org.netlib.util.intW
import vecxt.matrix.Matrix
import vecxt.MatrixInstance.*
import vecxt.MatrixHelper.zeros
import vecxt.BoundsCheck.BoundsCheck

object QR:
  private lazy final val lapack = JavaLAPACK.getInstance()

  /** Computes the QR decomposition of a matrix using LAPACK's dgeqrf and dorgqr routines.
    *
    * The QR decomposition factorizes a matrix A into the product Q * R, where:
    *   - Q is an orthogonal matrix (Q^T * Q = I)
    *   - R is an upper triangular matrix
    *
    * For an m×n matrix A:
    *   - Q is m×m (orthogonal)
    *   - R is m×n (upper triangular)
    *
    * This is useful for solving least squares problems, computing matrix rank, and other numerical linear algebra
    * tasks.
    *
    * @param matrix
    *   The input matrix to decompose. Must have positive dimensions.
    * @return
    *   A named tuple containing:
    *   - Q: The orthogonal matrix (m×m)
    *   - R: The upper triangular matrix (m×n)
    * @throws IllegalArgumentException
    *   if matrix dimensions are not positive or if an argument to LAPACK is invalid
    * @throws ArithmeticException
    *   if the QR decomposition fails to compute
    */
  inline def qr(matrix: Matrix[Double])(using
      inline bc: BoundsCheck
  ): (Q: Matrix[Double], R: Matrix[Double]) =
    val (m, n) = matrix.shape

    nonEmptyMatCheck(matrix)

    val minmn = math.min(m, n)

    // Make a copy of the input matrix (LAPACK overwrites it)
    val aCopy = matrix.deepCopy

    // Array to store the scalar factors of the elementary reflectors
    val tau = Array.ofDim[Double](minmn)

    // Query for optimal workspace size
    val workQuery = Array.ofDim[Double](1)
    val info = new intW(0)

    lapack.dgeqrf(
      m,
      n,
      Array.empty[Double],
      scala.math.max(1, m),
      Array.empty[Double],
      workQuery,
      -1,
      info
    )

    if info.`val` != 0 then throw IllegalStateException(s"QR workspace query failed. INFO=${info.`val`}")
    end if

    // Allocate workspace
    val lwork = math.max(1, workQuery(0).toInt)
    val work = Array.ofDim[Double](lwork)
    info.`val` = 0

    // Compute QR factorization
    lapack.dgeqrf(
      m,
      n,
      aCopy.raw,
      scala.math.max(1, m),
      tau,
      work,
      lwork,
      info
    )

    if info.`val` < 0 then
      throw IllegalArgumentException(s"QR factorization failed: the ${-info.`val`}th argument had an illegal value")
    else if info.`val` > 0 then throw ArithmeticException(s"QR factorization failed. INFO=${info.`val`}")
    end if

    // Extract R (upper triangular part of aCopy)
    val rMatrix = Matrix.zeros[Double](m, n)
    var i = 0
    while i < m do
      var j = i
      while j < n do
        rMatrix(i, j) = aCopy(i, j)
        j += 1
      end while
      i += 1
    end while

    // Generate Q from the elementary reflectors stored in aCopy
    // For generating the full Q matrix (m x m), we need to work with the first minmn columns
    // and then extend to a full orthogonal matrix

    // Create a new matrix to store Q (m x m)
    val qData = Array.ofDim[Double](m * m)

    // Copy the first minmn columns from aCopy to qData
    var col = 0
    while col < minmn do
      var row = 0
      while row < m do
        qData(row + col * m) = aCopy(row, col)
        row += 1
      end while
      col += 1
    end while

    // Query workspace for dorgqr
    info.`val` = 0
    lapack.dorgqr(
      m,
      m,
      minmn,
      Array.empty[Double],
      scala.math.max(1, m),
      Array.empty[Double],
      workQuery,
      -1,
      info
    )

    if info.`val` != 0 then throw IllegalStateException(s"Q generation workspace query failed. INFO=${info.`val`}")
    end if

    val lworkQ = math.max(1, workQuery(0).toInt)
    val workQ = Array.ofDim[Double](lworkQ)
    info.`val` = 0

    // Generate the orthogonal matrix Q
    lapack.dorgqr(
      m,
      m,
      minmn,
      qData,
      scala.math.max(1, m),
      tau,
      workQ,
      lworkQ,
      info
    )

    if info.`val` < 0 then
      throw IllegalArgumentException(s"Q generation failed: the ${-info.`val`}th argument had an illegal value")
    else if info.`val` > 0 then throw ArithmeticException(s"Q generation failed. INFO=${info.`val`}")
    end if

    val qMatrix = Matrix(qData, m, m)(using false)

    (Q = qMatrix, R = rMatrix)
  end qr

end QR
