package vecxt

import vecxt.BoundsCheck.BoundsCheck
import vecxt.matrix.*

import narr.*

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
  inline def apply[A](raw: NArray[A], dim: RowCol)(using inline doCheck: BoundsCheck) =
    inline if doCheck then
      if dim._1 * dim._2 != raw.size
      then throw InvalidMatrix(dim._1, dim._2, raw.size)
end dimMatInstantiateCheck

object dimMatDInstantiateCheck:
  inline def apply[A](raw: narr.native.DoubleArray, dim: RowCol)(using inline doCheck: BoundsCheck) =
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
