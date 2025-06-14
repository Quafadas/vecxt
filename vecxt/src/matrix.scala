package vecxt
import vecxt.BoundsCheck.BoundsCheck

import narr.*

object matrix:

  /** This is a matrix
    *
    * ._1 is the Matrix[A] values, stored as a single contiguous array ._2 is the number of rows ._3 is the number of
    * columns. You can access the raw array with the .raw method which inlines to the tuple call.
    *
    * Storage is column major.
    */
  opaque type Matrix[@specialized(Double, Boolean, Int) A] = (NArray[A], Row, Col)

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
      (raw, dim._1, dim._2)
    end apply
    inline def apply[@specialized(Double, Boolean, Int) A](dim: RowCol, raw: NArray[A])(using
        inline boundsCheck: BoundsCheck
    ): Matrix[A] =
      dimMatInstantiateCheck(raw, dim)
      (raw, dim._1, dim._2)
    end apply
  end Matrix

  extension [@specialized(Double, Boolean, Int) A](m: Matrix[A])

    transparent inline def raw = m._1

    inline def shape: RowCol = (m._2, m._3)

    inline def rows: Row = m._2

    inline def cols: Col = m._3

    inline def numel: Int = m._1.length

  end extension

end matrix
