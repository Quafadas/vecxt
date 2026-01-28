package vecxt

import vecxt.BoundsCheck.BoundsCheck
import vecxt.all.*

object Eigenvalues:
  inline def eig(m: Matrix[Double])(using
      inline bc: BoundsCheck
  ): (eigenvalues: Array[Double], complexEigenValues: Array[Double], eigenVectors: Matrix[Double]) = ???
end Eigenvalues
