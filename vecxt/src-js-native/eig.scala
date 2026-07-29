package vecxt

import vecxt.all.*

object Eigenvalues:
  inline def eig(
      m: Matrix[Double]
  ): (eigenvalues: Array[Double], complexEigenValues: Array[Double], eigenVectors: Matrix[Double]) = 
    scala.compiletime.error(
      "Unimplemented on JS / Native at the moment"
    )
end Eigenvalues
