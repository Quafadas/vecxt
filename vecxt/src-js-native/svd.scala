package vecxt

import vecxt.matrix.Matrix

// https://github.com/scalanlp/breeze/blob/fd73d09976a1a50d68b91a53e3896980502d335e/math/src/main/scala/breeze/linalg/functions/svd.scala#L13
object Svd:
  //  Options fot the singular value decomposition (SVD) of a real M-by-N matrix
  enum SVDMode:
    case CompleteSVD // all M columns of U and all N rows of V**T are returned in the arrays U and VT
    case ReducedSVD // the first min(M,N) columns of U and the first min(M,N) rows of V**T are returned in the arrays U and VT
  end SVDMode

  inline def pinv(matrix: Matrix[Double], toleranceFactor: Double = 1.0): Matrix[Double] = ???

  inline def rank(matrix: Matrix[Double], toleranceFactor: Double = 1.0): Int = ???

  /** Computes the Singular Value Decomposition (SVD) of a matrix using LAPACK's dgesdd routine.
    *
    * The SVD decomposes a matrix A into the product U * Σ * Vt, where:
    *   - U is an orthogonal matrix containing the left singular vectors
    *   - Σ is a diagonal matrix containing the singular values (returned as an array s)
    *   - Vt is an orthogonal matrix containing the right singular vectors (transposed)
    *
    * @param matrix
    *   The input matrix to decompose. Must have positive dimensions.
    * @param mode
    *   The SVD computation mode. Defaults to CompleteSVD.
    *   - CompleteSVD: computes full-sized U (m×m) and Vt (n×n) matrices
    *   - ReducedSVD: computes reduced U (m×min(m,n)) and Vt (min(m,n)×n) matrices
    * @return
    *   A named tuple containing:
    *   - U: The left singular vectors matrix
    *   - s: Array of singular values in descending order
    *   - Vt: The right singular vectors matrix (transposed)
    * @throws IllegalArgumentException
    *   if matrix dimensions are not positive or if an argument to LAPACK is invalid
    * @throws IllegalStateException
    *   if the SVD computation fails to converge
    */
  inline def svd(
      matrix: Matrix[Double],
      mode: SVDMode = SVDMode.CompleteSVD
  ): (U: Matrix[Double], s: Array[Double], Vt: Matrix[Double]) = ???

end Svd
