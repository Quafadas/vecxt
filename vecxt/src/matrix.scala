package vecxt
import vecxt.BoundsCheck.BoundsCheck

import narr.*

object matrix:


  /** This is a matrix
    *
    * Storage is column major.
    */
  class Matrix[@specialized(Double, Boolean, Int) A](val raw: NArray[A], val rows: Row, val cols: Col)

  object Matrix:

    // inline def apply(raw: narr.native.DoubleArray, dim: RowCol)(using
    //     inline boundsCheck: BoundsCheck
    // ): Matrix[Double] =
    //   dimMatDInstantiateCheck(raw, dim)
    //   (raw, dim)
    // end apply

    inline def apply[@specialized(Double, Boolean, Int) A](raw: NArray[A], dim: RowCol)(using
        inline boundsCheck: BoundsCheck
    ): Matrix[A] =
      dimMatInstantiateCheck(raw, dim)
      new Matrix(raw, dim._1, dim._2)
    end apply
    inline def apply[@specialized(Double, Boolean, Int) A](dim: RowCol, raw: NArray[A])(using
        inline boundsCheck: BoundsCheck
    ): Matrix[A] =
      dimMatInstantiateCheck(raw, dim)
      new Matrix(raw, dim._1, dim._2)
    end apply
  end Matrix

  extension [@specialized(Double, Boolean, Int) A](m: Matrix[A])

    // transparent inline def refinedRaw = m.raw

    inline def shape: RowCol = (m.rows, m.cols)

    // inline def rows: Row = m._2

    // inline def cols: Col = m._3

    inline def numel: Int = m.raw.length

  end extension

end matrix
