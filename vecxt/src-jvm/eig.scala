package vecxt

import dev.ludovic.netlib.lapack.JavaLAPACK
import org.netlib.util.intW
import vecxt.matrix.Matrix
import vecxt.MatrixInstance.*
import vecxt.MatrixHelper.zeros
import vecxt.BoundsCheck.BoundsCheck

// https://github.com/scalanlp/breeze/blob/fd73d09976a1a50d68b91a53e3896980502d335e/math/src/main/scala/breeze/linalg/functions/eig.scala#L25
object Eigenvalues:
  private lazy final val lapack = JavaLAPACK.getInstance()

  inline def eig(m: Matrix[Double])(using
      inline bc: BoundsCheck
  ): (eigenvalues: Array[Double], complexEigenValues: Array[Double], eigenVectors: Matrix[Double]) =
    nonEmptyMatCheck(m)
    squareMatCheck(m)
    if bc == vecxt.BoundsCheck.DoBoundsCheck.yes then
      require(!m.raw.exists(_.isNaN), "Input matrix contains NaN values")
    end if

    val n = m.rows

    // Allocate space for the decomposition
    val Wr = Array.fill[Double](n)(0.0)
    val Wi = Array.fill[Double](n)(0.0)

    val Vr = Matrix.zeros[Double](n, n)
    val Vl = Matrix.zeros[Double](n, n)
    // Find the needed workspace
    val worksize = Array.ofDim[Double](1)
    val info = new intW(0)

    lapack.dgeev(
      "N",
      "V",
      n,
      Array.empty[Double],
      scala.math.max(1, n),
      Array.empty[Double],
      Array.empty[Double],
      Array.empty[Double],
      scala.math.max(1, n),
      Array.empty[Double],
      scala.math.max(1, n),
      worksize,
      -1,
      info
    )

    // Allocate the workspace
    val lwork: Int =
      if info.`val` != 0 then scala.math.max(1, 4 * n)
      else scala.math.max(1, worksize(0).toInt)

    val work = Array.ofDim[Double](lwork)

    // Factor it!

    val A = m.deepCopy

    lapack.dgeev(
      "N",
      "V",
      n,
      A.raw,
      scala.math.max(1, n),
      Wr,
      Wi,
      Vl.raw,
      scala.math.max(1, n),
      Vr.raw,
      scala.math.max(1, n),
      work,
      work.length,
      info
    )

    if info.`val` > 0 then throw new ArithmeticException("Eigenvalue computation did not converge")
    else if info.`val` < 0 then throw new IllegalArgumentException()
    end if

    (eigenvalues = Wr, complexEigenValues = Wi, eigenVectors = Vr)
  end eig
end Eigenvalues
