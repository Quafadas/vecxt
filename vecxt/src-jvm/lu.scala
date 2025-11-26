package vecxt

import dev.ludovic.netlib.lapack.JavaLAPACK
import org.netlib.util.intW
import vecxt.matrix.Matrix
import vecxt.MatrixInstance.*
import vecxt.MatrixHelper.zeros
import vecxt.BoundsCheck.BoundsCheck

/** LU decomposition with partial pivoting using LAPACK.
  *
  * Decomposes a matrix A into the product P*A = L*U where:
  *   - P is a permutation matrix
  *   - L is a lower triangular matrix with unit diagonal
  *   - U is an upper triangular matrix
  */
object LU:
  private lazy final val lapack = JavaLAPACK.getInstance()

  /** Computes the LU decomposition with partial pivoting of a matrix.
    *
    * Uses LAPACK's dgetrf routine to compute the factorization P*A = L*U where:
    *   - P is a permutation matrix (represented as a pivot array)
    *   - L is a lower triangular matrix with ones on the diagonal
    *   - U is an upper triangular matrix
    *
    * The decomposition allows efficient solving of linear systems and computation of determinants.
    *
    * Note: The factorization is computed even for singular matrices. In such cases, at least one diagonal element of U
    * will be zero (or near zero), indicating the matrix is rank-deficient.
    *
    * @param m
    *   The input matrix to decompose. Can be square or rectangular (m x n).
    * @return
    *   A named tuple containing:
    *   - L: Lower triangular matrix with unit diagonal (rows × min(rows,cols))
    *   - U: Upper triangular matrix (min(rows,cols) × cols)
    *   - P: Permutation array where P(i) = j means row i was swapped with row j. The permutation matrix can be
    *     reconstructed from this array.
    * @throws IllegalArgumentException
    *   if matrix is empty or if an argument to LAPACK is invalid
    */
  inline def lu(m: Matrix[Double])(using
      inline bc: BoundsCheck
  ): (L: Matrix[Double], U: Matrix[Double], P: Array[Int]) =
    nonEmptyMatCheck(m)

    val rows = m.rows
    val cols = m.cols
    val minDim = math.min(rows, cols)

    // Copy input matrix as LAPACK will overwrite it
    val A = m.deepCopy

    // Allocate pivot array - LAPACK uses 1-based indexing
    val ipiv = Array.ofDim[Int](minDim)

    val info = new intW(0)

    // Compute LU factorization
    lapack.dgetrf(
      rows, // M: number of rows
      cols, // N: number of columns
      A.raw, // A: matrix to factor (overwritten with L and U)
      math.max(1, rows), // LDA: leading dimension
      ipiv, // IPIV: pivot indices
      info // INFO: status
    )

    // Check for errors
    if info.`val` < 0 then
      throw new IllegalArgumentException(s"LU decomposition failed: argument ${-info.`val`} had an illegal value")
    end if

    // Note: info.val > 0 means U(info.val, info.val) is exactly zero
    // The factorization has been completed, but U is singular
    // We allow this and let the caller handle it

    // Extract L and U from the factored matrix
    // LAPACK stores L and U in the same matrix:
    // - U is in the upper triangle (including diagonal)
    // - L is in the lower triangle (diagonal is implicitly 1)

    val L = Matrix.zeros[Double](rows, minDim)
    val U = Matrix.zeros[Double](minDim, cols)

    // Extract L (lower triangle with unit diagonal)
    var i = 0
    while i < rows do
      var j = 0
      while j < minDim do
        if i > j then
          // Below diagonal: copy from A
          L(i, j) = A(i, j)
        else if i == j then
          // On diagonal: set to 1
          L(i, j) = 1.0
        // else: above diagonal is 0 (already initialized)
        end if
        j += 1
      end while
      i += 1
    end while

    // Extract U (upper triangle)
    i = 0
    while i < minDim do
      var j = 0
      while j < cols do
        if i <= j then
          // On or above diagonal: copy from A
          U(i, j) = A(i, j)
        // else: below diagonal is 0 (already initialized)
        end if
        j += 1
      end while
      i += 1
    end while

    // Convert LAPACK's 1-based pivot indices to 0-based
    val pivotArray = Array.ofDim[Int](minDim)
    i = 0
    while i < minDim do
      pivotArray(i) = ipiv(i) - 1 // Convert from 1-based to 0-based
      i += 1
    end while

    (L = L, U = U, P = pivotArray)
  end lu

end LU
