package vecxt

import vecxt.Tensors.Matrix

protected[vecxt] object dimMatCheck:
  inline def apply(a: Matrix, b: Matrix)(using inline doCheck: BoundsCheck) =
    inline if doCheck then if a.cols != b.rows then throw MatrixDimensionMismatch(a.rows, a.cols, b.rows, b.cols)
end dimMatCheck

protected[vecxt] object dimMatInstantiateCheck:
  inline def apply(a: Matrix)(using inline doCheck: BoundsCheck) =
    inline if doCheck then
      if a.cols * a.rows != a.raw.size
      then throw InvalidMatrix(a.rows, a.cols, a.raw.size)
end dimMatInstantiateCheck

case class MatrixDimensionMismatch(aCols: Int, aRows: Int, bCols: Int, bRows: Int)
    extends Exception(
      s"Matrix dimensions do not match. Matrix A : ($aRows, $aCols), Matrix B : ($bRows, $bCols)"
    )

case class InvalidMatrix(cols: Int, rows: Int, data: Int)
    extends Exception(
      s"Matrix dimensions do not match. Matrix A : ($cols, $rows), is provided with data of length $data"
    )
