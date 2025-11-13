package vecxt

import dev.ludovic.netlib.lapack.JavaLAPACK
import org.netlib.util.intW
import vecxt.matrix.Matrix
import vecxt.MatrixInstance.deepCopy

// https://github.com/scalanlp/breeze/blob/fd73d09976a1a50d68b91a53e3896980502d335e/math/src/main/scala/breeze/linalg/functions/svd.scala#L13
object Svd:
  //  Options fot the singular value decomposition (SVD) of a real M-by-N matrix
  enum SVDMode:
    case CompleteSVD // all M columns of U and all N rows of V**T are returned in the arrays U and VT
    case ReducedSVD // the first min(M,N) columns of U and the first min(M,N) rows of V**T are returned in the arrays U and VT
  end SVDMode

  private val lapack = JavaLAPACK.getInstance()

  def svd(
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
