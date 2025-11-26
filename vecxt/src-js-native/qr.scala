package vecxt

import vecxt.matrix.Matrix

object QR:
  /** Computes the QR decomposition of a matrix.
    *
    * The QR decomposition factorizes a matrix A into the product Q * R, where:
    *   - Q is an orthogonal matrix (Q^T * Q = I)
    *   - R is an upper triangular matrix
    *
    * @param matrix
    *   The input matrix to decompose. Must have positive dimensions.
    * @return
    *   A named tuple containing:
    *   - Q: The orthogonal matrix
    *   - R: The upper triangular matrix
    */
  inline def qr(matrix: Matrix[Double]): (Q: Matrix[Double], R: Matrix[Double]) = ???

end QR
