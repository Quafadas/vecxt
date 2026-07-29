package vecxt

import vecxt.matrix.Matrix

object Cholesky:
  inline def cholesky(matrix: Matrix[Double], toleranceFactor: Double = 1.0): Int = scala.compiletime.error(
    "Unimplemented on JS / Native at the moment"
  )
end Cholesky
