package vecxt

import narr.*
import jdk.incubator.vector.DoubleVector
import vecxt.arrays.*
import vecxt.BoundsCheck.BoundsCheck
import org.ekrich.blas.unsafe.blas
import org.ekrich.blas.unsafe.blasEnums
import scala.scalanative.unsafe.*
import vecxt.matrixUtil.*

object matrix:
  /** This is a matrix
    *
    * ._1 is the matrix values, stored as a single contiguous array ._2 is the dimensions ._2._1 is the number of rows
    * ._2._2 is the number of columns
    */
  opaque type Matrix = (NArray[Double], Tuple2[Int, Int])

  type RangeExtender = Range | Int | NArray[Int] | ::.type

  // type Matrix = Matrix1 & Tensor

  object Matrix:

    inline def apply[T <: Tuple2[Int, Int]](raw: NArray[Double], dim: T)(using
        inline boundsCheck: BoundsCheck
    ): Matrix =
      dimMatInstantiateCheck(raw, dim)
      (raw, dim)
    end apply
    inline def apply[T <: Tuple2[Int, Int]](dim: T, raw: NArray[Double])(using
        inline boundsCheck: BoundsCheck
    ): Matrix =
      dimMatInstantiateCheck(raw, dim)
      (raw, dim)
    end apply

    inline def fromRows(a: NArray[NArray[Double]])(using inline boundsCheck: BoundsCheck): Matrix =
      val rows = a.size
      val cols = a(0).size

      assert(a.forall(_.size == cols))

      val newArr = NArray.ofSize[Double](rows * cols)
      var idx = 0
      var i = 0
      while i < cols do
        var j = 0
        while j < rows do
          newArr(idx) = a(j)(i)
          // println(s"row: $i || col: $j || ${a(j)(i)} entered at index ${idx}")
          j += 1
          idx += 1
        end while
        i += 1
      end while
      Matrix(newArr, (rows, cols))
    end fromRows

    inline def ones(dim: Tuple2[Int, Int]): Matrix =
      val (rows, cols) = dim
      val newArr = NArray.fill[Double](rows * cols)(1.0)
      Matrix(newArr, dim)(using BoundsCheck.DoBoundsCheck.no)
    end ones

    inline def zeros(dim: Tuple2[Int, Int]): Matrix =
      val (rows, cols) = dim
      val newArr = NArray.ofSize[Double](rows * cols)
      Matrix(newArr, dim)(using BoundsCheck.DoBoundsCheck.no)
    end zeros

    inline def eye(dim: Int): Matrix =
      val newArr = NArray.ofSize[Double](dim * dim)
      var i = 0
      while i < dim do
        newArr(i * dim + i) = 1.0
        i += 1
      end while
      Matrix(newArr, (dim, dim))(using BoundsCheck.DoBoundsCheck.no)
    end eye

    inline def fromColumns(a: NArray[NArray[Double]])(using inline boundsCheck: BoundsCheck): Matrix =
      val cols = a.size
      val rows = a(0).size
      assert(a.forall(_.size == rows))
      val newArr = NArray.ofSize[Double](rows * cols)
      var idx = 0
      var i = 0
      while i < cols do
        var j = 0
        while j < rows do
          // val idx = i * cols + j
          newArr(idx) = a(i)(j)
          // println(s"i: $i || j: $j || ${a(i)(j)} entered at index ${idx}")
          j += 1
          idx += 1
        end while
        i += 1
      end while
      Matrix(newArr, (rows, cols))
    end fromColumns

  end Matrix

  extension (m: Matrix)

    inline def numel: Int = m._1.length

    inline def tupleFromIdx(b: Int)(using inline boundsCheck: BoundsCheck) =
      dimCheckLen(m.raw, b)
      (b / m.rows, b % m.rows)
    end tupleFromIdx

    /** element update
      */
    inline def update(loc: Tuple2[Int, Int], value: Double)(using inline boundsCheck: BoundsCheck) =
      indexCheckMat(m, loc)
      val idx = loc._2 * m.rows + loc._1
      m._1(idx) = value
    end update

    private inline def range(r: RangeExtender, max: Int): NArray[Int] = r match
      case _: ::.type     => NArray.from((0 until max).toArray)
      case r: Range       => NArray.from(r.toArray)
      case l: NArray[Int] => l
      case i: Int         => NArray(i)

    def apply(rowRange: RangeExtender, colRange: RangeExtender): Matrix =
      val newRows = range(rowRange, m.rows)
      val newCols = range(colRange, m.cols)
      val newArr = NArray.ofSize[Double](newCols.size * newRows.size)

      var idx = 0

      var i = 0
      while i < newCols.length do
        val colpos = newCols(i)
        val stride = colpos * m.cols
        var j = 0
        while j < newRows.length do
          val rowPos = newRows(j)
          newArr(idx) = m._1(stride + rowPos)
          idx += 1
          j += 1
        end while
        i += 1
      end while

      Matrix(newArr, (newRows.size, newCols.size))(using BoundsCheck.DoBoundsCheck.no)

    end apply

    /** element retrieval
      */
    inline def apply(b: Tuple2[Int, Int])(using inline boundsCheck: BoundsCheck) =
      indexCheckMat(m, b)
      val idx = b._2 * m.rows + b._1
      m._1(idx)
    end apply

    inline def raw: NArray[Double] = m._1

    inline def +(m2: Matrix)(using inline boundsCheck: BoundsCheck): Matrix =
      sameDimMatCheck(m, m2)
      val newArr = m._1.add(m2._1)
      Matrix(newArr, m._2)(using BoundsCheck.DoBoundsCheck.no)
    end +

    inline def rows: Int = m._2._1

    inline def cols: Int = m._2._2

    inline def matmul(b: Matrix)(using inline boundsCheck: BoundsCheck): Matrix =
      dimMatCheck(m, b)
      val newArr = Array.ofDim[Double](m.rows * b.cols)
      // Note, might need to deal with transpose later.
      blas.cblas_dgemm(
        blasEnums.CblasColMajor,
        blasEnums.CblasNoTrans,
        blasEnums.CblasNoTrans,
        m.rows,
        b.cols,
        m.cols,
        1.0,
        m.raw.at(0),
        m.rows,
        b.raw.at(0),
        b.rows,
        1.0,
        newArr.at(0),
        m.rows
      )
      Matrix(newArr, (m.rows, b.cols))
    end matmul

  end extension
end matrix
