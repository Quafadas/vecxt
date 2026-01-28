package vecxt

import vecxt.BoundsCheck.BoundsCheck
import vecxt.matrix.Matrix

/** Linear system solver placeholder for JS and Native platforms.
  *
  * Full implementation is available on JVM platform using LAPACK.
  */
object Solve:
  /** Solves a system of linear equations A*x = b using LU decomposition.
    *
    * @param A
    *   The coefficient matrix (n×n). Must be square and non-empty.
    * @param b
    *   The right-hand side matrix (n×m). The number of rows must equal the number of rows in A.
    * @return
    *   The solution matrix x (n×m) such that A*x = b.
    */
  inline def solve(A: Matrix[Double], b: Matrix[Double])(using inline bc: BoundsCheck): Matrix[Double] = ???

  /** Solves a system of linear equations A*x = b for a single right-hand side vector.
    *
    * @param A
    *   The coefficient matrix (n×n). Must be square and non-empty.
    * @param b
    *   The right-hand side vector as an Array[Double] of length n.
    * @return
    *   The solution vector x as an Array[Double] of length n such that A*x = b.
    */
  inline def solve(A: Matrix[Double], b: Array[Double])(using inline bc: BoundsCheck): Array[Double] = ???

end Solve
