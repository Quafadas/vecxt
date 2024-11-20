package vecxt
import vecxt.BoundsCheck.BoundsCheck

import narr.*

object matrix:

  /** This is a matrix
    *
    * ._1 is the Matrix[A] values, stored as a single contiguous array ._2 is the dimensions ._2._1 is the number of
    * rows ._2._2 is the number of columns. You can access the raw array with the .raw method which inlines to the tuple
    * call.
    *
    * Storage is column major.
    */
  opaque type Matrix[A] = (NArray[A], RowCol)

  object Matrix:

    inline def apply[@specialized(Double, Boolean, Int) A](raw: NArray[A], dim: RowCol)(using
        inline boundsCheck: BoundsCheck
    ): Matrix[A] =
      dimMatInstantiateCheck(raw, dim)
      (raw, dim)
    end apply
    inline def apply[@specialized(Double, Boolean, Int) A](dim: RowCol, raw: NArray[A])(using
        inline boundsCheck: BoundsCheck
    ): Matrix[A] =
      dimMatInstantiateCheck(raw, dim)
      (raw, dim)
    end apply
  end Matrix

  extension [@specialized(Double, Boolean, Int) A](m: Matrix[A])

    inline def raw: NArray[A] = m._1

    inline def shape: RowCol = m._2

  end extension

end matrix
