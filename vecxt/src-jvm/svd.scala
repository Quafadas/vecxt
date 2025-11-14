package vecxt

import dev.ludovic.netlib.lapack.JavaLAPACK
import org.netlib.util.intW
import vecxt.matrix.Matrix
import vecxt.MatrixInstance.deepCopy
import vecxt.arrays.maxSIMD
import vecxt.arrays.>
import vecxt.BooleanArrays.trues
import scala.annotation.targetName

// https://github.com/scalanlp/breeze/blob/fd73d09976a1a50d68b91a53e3896980502d335e/math/src/main/scala/breeze/linalg/functions/svd.scala#L13
lazy object Svd:
  private lazy final val lapack = JavaLAPACK.getInstance()
  //  Options fot the singular value decomposition (SVD) of a real M-by-N matrix
  enum SVDMode:
    case CompleteSVD // all M columns of U and all N rows of V**T are returned in the arrays U and VT
    case ReducedSVD // the first min(M,N) columns of U and the first min(M,N) rows of V**T are returned in the arrays U and VT
  end SVDMode



  /** Computes the rank of a matrix using Singular Value Decomposition (SVD).
    *
    * The rank is determined by counting the number of singular values that exceed
    * a computed tolerance threshold. The tolerance is calculated using the same
    * formula as LAPACK and NumPy: `tol = maxSV * max(m, n) * eps * toleranceFactor`,
    * where `maxSV` is the largest singular value, `m` and `n` are the matrix dimensions,
    * and `eps` is the machine epsilon for Double precision.
    *
    * @param matrix the input matrix for which to compute the rank
    * @param toleranceFactor a multiplier for the tolerance threshold (default: 1.0).
    *                        Increasing this value makes the rank calculation more conservative
    *                        by requiring singular values to be larger to be counted.
    * @return the numerical rank of the matrix, i.e., the number of singular values
    *         above the tolerance threshold
    */
  inline def rank(matrix: Matrix[Double],
      toleranceFactor: Double = 1.0)
    : Int =
    val (_, singularValues, _) = svd(matrix)

    // Machine epsilon for Double
    val eps = 2.220446049250313e-16

    // Largest singular value
    val maxSv = if (singularValues.nonEmpty) singularValues.maxSIMD else 0.0

    // SVD-based tolerance (same formula used by LAPACK / NumPy)
    val tol = maxSv * math.max(matrix.rows, matrix.cols) * eps * toleranceFactor

    // Count singular values above tolerance
    (singularValues > tol).trues



  /** Computes the Singular Value Decomposition (SVD) of a matrix using LAPACK's dgesdd routine.
    *
    * The SVD decomposes a matrix A into the product U * Σ * Vt, where:
    * - U is an orthogonal matrix containing the left singular vectors
    * - Σ is a diagonal matrix containing the singular values (returned as an array s)
    * - Vt is an orthogonal matrix containing the right singular vectors (transposed)
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
  ): (U: Matrix[Double], s: Array[Double], Vt: Matrix[Double]) =
    val (m, n) = matrix.shape
    require(m > 0 && n > 0, s"Matrix dimensions must be positive, got ($m, $n)")

    val minmn = math.min(m, n)
    val jobz = mode match
      case SVDMode.CompleteSVD => "A"
      case SVDMode.ReducedSVD  => "S"

    val aCopy = matrix.deepCopy
    val singularValues = Array.ofDim[Double](minmn)

    val uCols = mode match
      case SVDMode.CompleteSVD => m
      case SVDMode.ReducedSVD  => minmn

    val vtRows = mode match
      case SVDMode.CompleteSVD => n
      case SVDMode.ReducedSVD  => minmn

    val lda = math.max(1, m)
    val ldu = math.max(1, m)
    val ldvt = math.max(1, vtRows)

    val uArr = Array.ofDim[Double](ldu * uCols)
    val vtArr = Array.ofDim[Double](ldvt * n)

    val workQuery = Array.ofDim[Double](1)
    val iwork = Array.ofDim[Int](8 * minmn)
    val info = new intW(0)

    lapack.dgesdd(
      jobz,
      m,
      n,
      aCopy.raw,
      0,
      lda,
      singularValues,
      0,
      uArr,
      0,
      ldu,
      vtArr,
      0,
      ldvt,
      workQuery,
      0,
      -1,
      iwork,
      0,
      info
    )

    if info.`val` != 0 then throw IllegalStateException(s"SVD workspace query failed. INFO=${info.`val`}")
    end if

    val optimalWork = math.max(1, workQuery(0).toInt)
    val work = Array.ofDim[Double](optimalWork)
    info.`val` = 0

    lapack.dgesdd(
      jobz,
      m,
      n,
      aCopy.raw,
      0,
      lda,
      singularValues,
      0,
      uArr,
      0,
      ldu,
      vtArr,
      0,
      ldvt,
      work,
      0,
      optimalWork,
      iwork,
      0,
      info
    )

    if info.`val` < 0 then
      throw IllegalArgumentException(s"SVD failed: the ${-info.`val`}th argument had an illegal value")
    else if info.`val` > 0 then throw IllegalStateException(s"SVD failed to converge. INFO=${info.`val`}")
    end if

    val uMatrix = Matrix(uArr, m, uCols)(using false)
    val vtMatrix = Matrix(vtArr, vtRows, n)(using false)
    (U = uMatrix, s = singularValues, Vt = vtMatrix)
  end svd

end Svd
