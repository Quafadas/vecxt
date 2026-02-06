package vecxt

import vecxt.BoundsCheck.BoundsCheck
import vecxt.MatrixInstance.*
import vecxt.matrix.*

object dimMatCheck:
  inline def apply[A](a: Matrix[A], b: Matrix[A])(using inline doCheck: BoundsCheck) =
    inline if doCheck then if a.cols != b.rows then throw MatrixDimensionMismatch(a.rows, a.cols, b.rows, b.cols)
end dimMatCheck

object sameDimMatCheck:
  inline def apply[A, B](a: Matrix[A], b: Matrix[B])(using inline doCheck: BoundsCheck) =
    inline if doCheck then
      if !(a.cols == b.cols && a.rows == b.rows) then throw MatrixDimensionMismatch(a.rows, a.cols, b.rows, b.cols)
end sameDimMatCheck

/** If this is true, then we can use the same memory layout for element-wise operations
  */
object sameDenseElementWiseMemoryLayoutCheck:
  inline def apply[A, B](a: Matrix[A], b: Matrix[B])(using inline doCheck: BoundsCheck): Boolean =
    inline if doCheck then
      a.isDenseColMajor && b.isDenseColMajor && a.rowStride == b.rowStride || a.isDenseRowMajor && b.isDenseRowMajor && a.colStride == b.colStride
    else true
end sameDenseElementWiseMemoryLayoutCheck

object indexCheckMat:
  inline def apply[A](a: Matrix[A], dim: RowCol)(using inline doCheck: BoundsCheck) =
    inline if doCheck then
      if !(dim._1 >= 0 && dim._2 >= 0 && dim._1 <= a.rows && dim._2 <= a.cols) then
        throw java.lang.IndexOutOfBoundsException(
          s"Tried to update a ${a.rows} x ${a.cols} matrix at ${dim._1}, ${dim._2}, which is not valid. Please check your indexing."
        )
end indexCheckMat

object dimMatInstantiateCheck:
  inline def apply[A](raw: Array[A], dim: RowCol)(using inline doCheck: BoundsCheck) =
    inline if doCheck then
      if dim._1 * dim._2 != raw.size
      then throw InvalidMatrix(dim._1, dim._2, raw.size)
end dimMatInstantiateCheck

object nonEmptyMatCheck:
  inline def apply[A](mat: Matrix[A])(using inline doCheck: BoundsCheck) =
    inline if doCheck then if mat.cols == 0 || mat.rows == 0 then throw MatrixEmptyException()
end nonEmptyMatCheck

object squareMatCheck:
  inline def apply[A](mat: Matrix[A])(using inline doCheck: BoundsCheck) =
    inline if doCheck then if mat.rows != mat.cols then throw MatrixNotSquareException(mat.rows, mat.cols)
end squareMatCheck

object symmetricMatCheck:
  inline def apply(mat: Matrix[Double], tol: Double = 1e-7)(using inline doCheck: BoundsCheck) =
    inline if doCheck then
      squareMatCheck(mat)
      var i = 0
      while i < mat.rows do
        var j = 0
        while j < i do
          if math.abs(mat(i, j) - mat(j, i)) > tol then
            throw MatrixNotSymmetricException(mat.rows, mat.cols, i, j, mat(i, j), mat(j, i))
          end if
          j += 1
        end while
        i += 1
      end while
end symmetricMatCheck

case class MatrixEmptyException() extends Exception("Matrix must be non-empty")

case class MatrixNotSquareException(rows: Int, cols: Int)
    extends Exception(s"Matrix must be square, but has dimensions ($rows, $cols)")

case class MatrixNotSymmetricException(rows: Int, cols: Int, i: Int, j: Int, valueIJ: Double, valueJI: Double)
    extends Exception(
      s"Matrix must be symmetric, but ($rows, $cols) matrix has mat($i, $j) = $valueIJ != mat($j, $i) = $valueJI"
    )

object dimMatDInstantiateCheck:
  inline def apply[A](raw: Array[Double], dim: RowCol)(using inline doCheck: BoundsCheck) =
    inline if doCheck then
      if dim._1 * dim._2 != raw.size
      then throw InvalidMatrix(dim._1, dim._2, raw.size)
end dimMatDInstantiateCheck

case class MatrixDimensionMismatch(aCols: Int, aRows: Int, bCols: Int, bRows: Int)
    extends Exception(
      s"Matrix dimensions do not match. Matrix A : ($aRows, $aCols), Matrix B : ($bRows, $bCols)"
    )

case class InvalidMatrix(cols: Int, rows: Int, data: Int)
    extends Exception(
      s"Matrix dimensions do not match. Matrix A : ($cols, $rows), is provided with data of length $data"
    )
