package vecxt

import dev.ludovic.netlib.lapack.JavaLAPACK
import org.netlib.util.intW
import vecxt.matrix.Matrix
import vecxt.MatrixInstance.*
import vecxt.BoundsCheck.BoundsCheck

/** Linear system solver using LAPACK.
  *
  * Provides methods to solve systems of linear equations Ax = b using LAPACK's dgesv routine.
  */
object Solve:
  private lazy final val lapack = JavaLAPACK.getInstance()

  /** Solves a system of linear equations A*x = b using LU decomposition.
    *
    * This function uses LAPACK's dgesv routine to solve the linear system Ax = b where:
    *   - A is an n×n square matrix
    *   - b is an n×m matrix representing m right-hand side vectors
    *   - x is the n×m solution matrix
    *
    * The matrix A is factored using LU decomposition with partial pivoting and row interchanges.
    *
    * @param A
    *   The coefficient matrix (n×n). Must be square and non-empty.
    * @param b
    *   The right-hand side matrix (n×m). The number of rows must equal the number of rows in A.
    * @return
    *   The solution matrix x (n×m) such that A*x = b.
    * @throws IllegalArgumentException
    *   if matrix dimensions are incompatible or if an argument to LAPACK is invalid
    * @throws ArithmeticException
    *   if the matrix A is singular (not invertible)
    */
  inline def solve(A: Matrix[Double], b: Matrix[Double])(using
      inline bc: BoundsCheck
  ): Matrix[Double] =
    nonEmptyMatCheck(A)
    nonEmptyMatCheck(b)
    squareMatCheck(A)

    if bc == vecxt.BoundsCheck.DoBoundsCheck.yes then
      require(A.rows == b.rows, s"Matrix dimensions incompatible: A has ${A.rows} rows, b has ${b.rows} rows")
      require(!A.raw.exists(_.isNaN), "Input matrix A contains NaN values")
      require(!b.raw.exists(_.isNaN), "Input matrix b contains NaN values")
    end if

    val n = A.rows
    val nrhs = b.cols

    // LAPACK will overwrite A with LU factorization and b with solution
    // So we need to make copies
    val ACopy = A.deepCopy
    val x = b.deepCopy

    // Array for pivot indices
    val ipiv = Array.ofDim[Int](n)
    val info = new intW(0)

    lapack.dgesv(
      n,
      nrhs,
      ACopy.raw,
      scala.math.max(1, n),
      ipiv,
      x.raw,
      scala.math.max(1, n),
      info
    )

    if info.`val` < 0 then
      throw new IllegalArgumentException(s"LAPACK dgesv invalid parameter at position: ${-info.`val`}")
    else if info.`val` > 0 then
      throw new ArithmeticException(
        s"Matrix A is singular: U(${info.`val`},${info.`val`}) is exactly zero. The factorization has been completed, but the factor U is exactly singular, so the solution could not be computed."
      )
    end if

    x
  end solve

  /** Solves a system of linear equations A*x = b for a single right-hand side vector.
    *
    * Convenience method for solving Ax = b where b is a vector (n×1).
    *
    * @param A
    *   The coefficient matrix (n×n). Must be square and non-empty.
    * @param b
    *   The right-hand side vector as an Array[Double] of length n.
    * @return
    *   The solution vector x as an Array[Double] of length n such that A*x = b.
    * @throws IllegalArgumentException
    *   if matrix dimensions are incompatible or if an argument to LAPACK is invalid
    * @throws ArithmeticException
    *   if the matrix A is singular (not invertible)
    */
  inline def solve(A: Matrix[Double], b: Array[Double])(using inline bc: BoundsCheck): Array[Double] =
    if bc == vecxt.BoundsCheck.DoBoundsCheck.yes then
      require(A.rows == b.length, s"Matrix dimensions incompatible: A has ${A.rows} rows, b has ${b.length} elements")
    end if

    // Convert array to column matrix
    val bMatrix = Matrix(b, b.length, 1)(using false)
    val xMatrix = solve(A, bMatrix)

    // Extract solution as array
    val result = Array.ofDim[Double](A.rows)
    var i = 0
    while i < A.rows do
      result(i) = xMatrix(i, 0)
      i += 1
    end while
    result
  end solve

end Solve
