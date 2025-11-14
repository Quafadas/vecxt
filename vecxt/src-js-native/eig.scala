package vecxt

import all.*
import BoundsCheck.BoundsCheck

object Eigenvalues:
  inline def eig(m: Matrix[Double])(using
      inline bc: BoundsCheck
  ): (eigenvalues: Array[Double], complexEigenValues: Array[Double], eigenVectors: Matrix[Double]) = ???
end Eigenvalues
