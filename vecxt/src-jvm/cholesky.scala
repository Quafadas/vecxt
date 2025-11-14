package vecxt

import dev.ludovic.netlib.lapack.JavaLAPACK
import org.netlib.util.intW
import vecxt.matrix.Matrix

import vecxt.BoundsCheck.BoundsCheck
import vecxt.MatrixHelper.zeros
import vecxt.all.update
import vecxt.MatrixInstance.apply

// https://github.com/scalanlp/breeze/blob/fd73d09976a1a50d68b91a53e3896980502d335e/math/src/main/scala/breeze/linalg/functions/svd.scala#L13
object Cholesky:

  private lazy val lapack = JavaLAPACK.getInstance()

  // Copy the lower-triangular part of m into a new dense, column-major matrix.
  // LAPACK's dpotrf("L", ...) expects the input in this form and overwrites it with L.
  inline private def lowerTriangular(m: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
    val A = Matrix.zeros[Double](m.rows, m.cols)
    val N = m.rows
    var i = 0
    // TODO: Definitely SIMD optimisable under the right conditions
    while i < N do
      var j = 0
      while j <= i do
        A(i, j) = m(i, j)
        j += 1
      end while
      i += 1
    end while
    A
  end lowerTriangular

  inline def cholesky(m: Matrix[Double])(using inline boundsCheck: BoundsCheck) =
    nonEmptyMatCheck(m)

    symmetricMatCheck(m)

    // Copy the lower triangular part of m. LAPACK will store the result in result
    val result = lowerTriangular(m)

    val N = m.rows
    val info = new intW(0)
    lapack.dpotrf(
      "L",
      N,
      result.raw,
      scala.math.max(1, N),
      info
    )
    // A value of info.`val` < 0 would tell us that the i-th argument
    // of the call to dpotrf was erroneous (where i == |info.`val`|).
    if info.`val` < 0 then
      throw new IllegalArgumentException(s"LAPACK dpotrf invalid parameter at position: ${info.`val`}")
    end if

    if info.`val` > 0 then
      throw new ArithmeticException(
        s"did not converge: the leading minor of order ${info.`val`} is not positive definite"
      )
    end if

    result
  end cholesky

end Cholesky
