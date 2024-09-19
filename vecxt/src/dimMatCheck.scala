package vecxt

import vecxt.Matrix.*
import narr.*

protected[vecxt] object dimMatCheck:
  inline def apply(a: Matrix, b: Matrix)(using inline doCheck: BoundsCheck) =
    inline if doCheck then if a.cols != b.rows then throw MatrixDimensionMismatch(a.rows, a.cols, b.rows, b.cols)
end dimMatCheck

protected[vecxt] object sameDimMatCheck:
  inline def apply(a: Matrix, b: Matrix)(using inline doCheck: BoundsCheck) =
    inline if doCheck then
      if !(a.cols == b.cols && a.rows == b.rows) then throw MatrixDimensionMismatch(a.rows, a.cols, b.rows, b.cols)
end sameDimMatCheck

protected[vecxt] object indexCheckMat:
  inline def apply(a: Matrix, dim: Tuple2[Int, Int])(using inline doCheck: BoundsCheck) =
    inline if doCheck then
      if !(dim._1 >= 0 && dim._2 >= 0 && dim._1 <= a.rows && dim._2 <= a.cols) then
        throw java.lang.IndexOutOfBoundsException(a.rows, a.cols, b.rows, b.cols)
end indexCheckMat

protected[vecxt] object dimMatInstantiateCheck:
  inline def apply(raw: NArray[Double], dim: Tuple2[Int, Int])(using inline doCheck: BoundsCheck) =
    inline if doCheck then
      if dim._1 * dim._2 != raw.size
      then throw InvalidMatrix(dim._1, dim._2, raw.size)
end dimMatInstantiateCheck

case class MatrixDimensionMismatch(aCols: Int, aRows: Int, bCols: Int, bRows: Int)
    extends Exception(
      s"Matrix dimensions do not match. Matrix A : ($aRows, $aCols), Matrix B : ($bRows, $bCols)"
    )

case class InvalidMatrix(cols: Int, rows: Int, data: Int)
    extends Exception(
      s"Matrix dimensions do not match. Matrix A : ($cols, $rows), is provided with data of length $data"
    )
