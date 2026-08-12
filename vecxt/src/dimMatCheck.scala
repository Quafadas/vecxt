package vecxt

import vecxt.MatrixInstance.*
import vecxt.matrix.*
import scala.annotation.targetName

object dimMatCheck:
  inline def apply[A](a: Matrix[A], b: Matrix[A]) =
    if a.cols != b.rows then throw MatrixDimensionMismatch(a.rows, a.cols, b.rows, b.cols)
end dimMatCheck

object sameDimMatCheck:
  inline def apply[A, B](a: Matrix[A], b: Matrix[B]) =
    if !(a.cols == b.cols && a.rows == b.rows) then throw MatrixDimensionMismatch(a.rows, a.cols, b.rows, b.cols)
end sameDimMatCheck

/** Validates the output matrix `c` of a `matmulInPlace!` call: it must be shaped exactly `(m.rows, b.cols)` and dense
  * column-major. `matmulInPlace!` hardcodes `ldc = m.rows` and always writes (and, when `beta != 0`, reads) `c`
  * assuming that layout, so a wrongly-shaped or non-dense-column-major `c` would otherwise be corrupted or misread
  * silently instead of failing loudly. `matmul`/`@@` always build a conforming `c` themselves, so this only bites
  * direct callers of the in-place API.
  */
object matmulOutputCheck:
  inline def apply(m: Matrix[?], b: Matrix[?], c: Matrix[?]): Unit =
    if !(c.rows == m.rows && c.cols == b.cols) then throw MatrixDimensionMismatch(m.rows, b.cols, c.rows, c.cols)
    end if
    if !c.isDenseColMajor then
      throw UnsupportedLayoutException(
        s"matmulInPlace! requires a dense column-major output matrix `c`, but got layout: ${c.layoutString}"
      )
    end if
  end apply
end matmulOutputCheck

/** Whether a single BLAS leading dimension can describe `m`.
  *
  * BLAS addresses a matrix as one block in which one axis is contiguous and the other advances by a constant `lda` —
  * `a(offset + p + q * lda)`. So an operand is expressible exactly when one of its strides is `1` *and* the other is a
  * valid leading dimension for the extent BLAS will check it against.
  *
  * The second half is the part that is easy to drop, and dropping it is not harmless. `lda` must be at least the
  * block's row count or the routine rejects the call, and layouts satisfying `stride == 1` while failing it do occur: a
  * broadcast column has `colStride == 0`, repeating one column across the matrix, which no leading dimension expresses.
  * Guarding on `stride == 1` alone therefore lets a broadcast operand through to BLAS with `lda = 0`.
  *
  * Which extent applies follows from which stride is unit, because that also decides the transpose flag: an operand
  * with `rowStride == 1` is passed untransposed and its block has `rows` rows, so `colStride` must be at least `rows`;
  * one with `colStride == 1` is passed transposed, its block has `cols` rows, so `rowStride` must be at least `cols`.
  *
  * Callers that need to know *which* of the two cases holds — to pick the transpose flag and `lda` — should test the
  * halves directly rather than call this; this is for the guard that decides whether BLAS can be used at all.
  */
object blasLeadingDimensionCheck:
  def apply(m: Matrix[?]): Boolean =
    (m.rowStride == 1 && m.colStride >= m.rows) || (m.colStride == 1 && m.rowStride >= m.cols)
end blasLeadingDimensionCheck

/** If this is true, then we can use the same memory layout for element-wise operations
  */
object sameDenseElementWiseMemoryLayoutCheck:
  def apply[A, B](a: Matrix[A], b: Matrix[B]): Boolean =
    a.layout.sameElementOrderAs(b.layout)
end sameDenseElementWiseMemoryLayoutCheck

object indexCheckMat:

  inline def apply(a: Matrix[?], dim: RowCol) =
    if !(dim._1 >= 0 && dim._2 >= 0 && dim._1 < a.rows && dim._2 < a.cols) then
      throw java.lang.IndexOutOfBoundsException(
        s"Tried to update a ${a.rows} x ${a.cols} matrix at ${dim._1}, ${dim._2}, which is not valid. Please check your indexing."
      )

  @targetName("indexCheckMatInDouble")
  def apply(a: Matrix[Double], dim: RowCol) =
    if !(dim._1 >= 0 && dim._2 >= 0 && dim._1 < a.rows && dim._2 < a.cols) then
      throw java.lang.IndexOutOfBoundsException(
        s"Tried to update a ${a.rows} x ${a.cols} matrix at ${dim._1}, ${dim._2}, which is not valid. Please check your indexing."
      )

  @targetName("indexCheckMatInFloat")
  def apply(a: Matrix[Float], dim: RowCol) =
    if !(dim._1 >= 0 && dim._2 >= 0 && dim._1 < a.rows && dim._2 < a.cols) then
      throw java.lang.IndexOutOfBoundsException(
        s"Tried to update a ${a.rows} x ${a.cols} matrix at ${dim._1}, ${dim._2}, which is not valid. Please check your indexing."
      )

  @targetName("indexCheckMatInInt")
  def apply(a: Matrix[Int], dim: RowCol) =
    if !(dim._1 >= 0 && dim._2 >= 0 && dim._1 < a.rows && dim._2 < a.cols) then
      throw java.lang.IndexOutOfBoundsException(
        s"Tried to update a ${a.rows} x ${a.cols} matrix at ${dim._1}, ${dim._2}, which is not valid. Please check your indexing."
      )

  @targetName("indexCheckMatInLong")
  def apply(a: Matrix[Long], dim: RowCol) =
    if !(dim._1 >= 0 && dim._2 >= 0 && dim._1 < a.rows && dim._2 < a.cols) then
      throw java.lang.IndexOutOfBoundsException(
        s"Tried to update a ${a.rows} x ${a.cols} matrix at ${dim._1}, ${dim._2}, which is not valid. Please check your indexing."
      )
end indexCheckMat

object dimMatInstantiateCheck:
  inline def apply[A](raw: Array[A], dim: RowCol) =
    if dim._1 < 0 || dim._2 < 0 || dim._1 * dim._2 != raw.size
    then throw InvalidMatrix(dim._1, dim._2, raw.size)
end dimMatInstantiateCheck

object nonEmptyMatCheck:
  inline def apply[A](mat: Matrix[A]) =
    if mat.cols == 0 || mat.rows == 0 then throw MatrixEmptyException()
end nonEmptyMatCheck

object squareMatCheck:
  inline def apply[A](mat: Matrix[A]) =
    if mat.rows != mat.cols then throw MatrixNotSquareException(mat.rows, mat.cols)
end squareMatCheck

object symmetricMatCheck:
  inline def apply(mat: Matrix[Double], tol: Double = 1e-7) =
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
  end apply
end symmetricMatCheck

case class MatrixEmptyException() extends Exception("Matrix must be non-empty")

case class MatrixNotSquareException(rows: Int, cols: Int)
    extends Exception(s"Matrix must be square, but has dimensions ($rows, $cols)")

case class MatrixNotSymmetricException(rows: Int, cols: Int, i: Int, j: Int, valueIJ: Double, valueJI: Double)
    extends Exception(
      s"Matrix must be symmetric, but ($rows, $cols) matrix has mat($i, $j) = $valueIJ != mat($j, $i) = $valueJI"
    )

object dimMatDInstantiateCheck:
  inline def apply[A](raw: Array[Double], dim: RowCol) =
    if dim._1 < 0 || dim._2 < 0 || dim._1 * dim._2 != raw.size
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

case class UnsupportedLayoutException(message: String) extends Exception(message)

/** `DimensionExtender` (`Int | Dimension`) accepts any `Int`, not just the two meaningful values (`Rows`/`0`,
  * `Cols`/`1`) - so `reduceAlongDimension`'s `dim` can't be validated at compile time and a caller can genuinely reach
  * this at runtime, e.g. `mat.sum(2)`.
  */
case class InvalidDimensionException(dim: Int)
    extends Exception(s"Invalid dimension: $dim. Expected 0 (Rows) or 1 (Cols).")
