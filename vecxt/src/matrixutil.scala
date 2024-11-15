package vecxt

import narr.*
import vecxt.matrix.*
import vecxt.BoundsCheck
import vecxt.arrays.multInPlace
import vecxt.matrix.matmul
import vecxt.BoundsCheck.BoundsCheck
// import vecxt.arrayUtil.printArr
object matrixUtil:

  extension (m: Matrix)
    inline def transpose: Matrix =
      import vecxt.BoundsCheck.DoBoundsCheck.no
      val newMat = Matrix.zeros(m.cols, m.rows)
      var idx = 0

      while idx < newMat.numel do
        val positionNew = m.tupleFromIdx(idx)
        newMat(positionNew) = m((positionNew._2, positionNew._1))

        idx += 1
      end while
      newMat
    end transpose

    inline def row(i: Int): NArray[Double] =
      // println(s"row $i")
      m(i, ::).raw
    end row

    inline def printMat: String =
      val arrArr =
        for i <- 0 until m.rows
        yield
        // println(m.row(i).printArr)
        m.row(i).mkString(" ")
      end arrArr
      arrArr.mkString("\n")
    end printMat

    inline def col(i: Int): NArray[Double] =
      m(::, i).raw
    end col

    inline def @@(b: Matrix)(using inline boundsCheck: BoundsCheck): Matrix = m.matmul(b)

    inline def *:*=(d: Double): Unit = m.raw.multInPlace(d)

    inline def shape: String = s"${m.rows} x ${m.cols}"

  end extension
end matrixUtil
