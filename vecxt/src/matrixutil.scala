package vecxt

import narr.*
import vecxt.matrix.*
import vecxt.BoundsCheck
import vecxt.arrays.multInPlace
import vecxt.matrix.matmul
import vecxt.BoundsCheck.BoundsCheck

object matrixUtil:

  extension [A](d: Array[A]) def print: String = d.mkString("[", ",", "],")

  extension (m: Matrix)
    inline def transpose: Matrix =

      val newArr = NArray.ofSize[Double](m.numel)
      var i = 0
      while i < m.cols do
        var j = 0
        while j < m.rows do
          newArr(i * m.rows + j) = m.raw(j * m.cols + i)
          j += 1
        end while
        i += 1
      end while
      Matrix(newArr, (m.cols, m.rows))(using
        BoundsCheck.DoBoundsCheck.no
      ) // we already have a valid matrix if we are transposing it, so this check is redundant if this method works as intended.
    end transpose

    inline def row(i: Int): NArray[Double] =
      m(i, ::).raw
    end row

    inline def print: String =
      val arrArr = for i <- 0 until m.rows yield m.row(i).mkString(" ")
      arrArr.mkString("\n")
    end print

    inline def col(i: Int): NArray[Double] =
      m(::, i).raw
    end col

    inline def @@(b: Matrix)(using inline boundsCheck: BoundsCheck): Matrix = m.matmul(b)

    inline def *=(d: Double): Unit = m.raw.multInPlace(d)

  end extension
end matrixUtil
