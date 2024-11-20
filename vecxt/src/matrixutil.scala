package vecxt

import narr._
import vecxt.BoundsCheck
import vecxt.BoundsCheck.BoundsCheck
import vecxt.MatrixInstance._
import vecxt.arrayUtil.printArr
import vecxt.arrays.multInPlace
import vecxt.matrix._
import vecxt.matrix._

import scala.reflect.ClassTag
// import vecxt.arrayUtil.printArr
object matrixUtil:

  extension [A](m: Matrix[A])

    private inline def tupleFromIdx(b: Int)(using inline boundsCheck: BoundsCheck) =
      // dimCheckLen(m.raw, b)
      (b / m.rows, b % m.rows)
    end tupleFromIdx

    inline def transpose(using ClassTag[A]): Matrix[A] =
      import vecxt.BoundsCheck.DoBoundsCheck.no
      val newArr = NArray.ofSize[A](m.numel)
      val newMat = Matrix(newArr, (m.cols.asInstanceOf[Row], m.rows.asInstanceOf[Col]))
      var idx = 0

      while idx < newMat.numel do
        val positionNew = m.tupleFromIdx(idx)
        newMat(positionNew) = m((positionNew._2, positionNew._1))

        idx += 1
      end while
      newMat
    end transpose

    inline def row(i: Int)(using ClassTag[A]): NArray[A] =
      // println(s"row $i")
      val m2 = m(i, ::)
      m2.raw
    end row

    inline def printMat(using ClassTag[A]): String =
      val arrArr =
        for i <- 0 until m.rows
        yield
        // println(m.row(i).printArr)
        m.row(i).asInstanceOf[Array[Double]].printArr
      end arrArr
      arrArr.mkString("\n")
    end printMat

    inline def col(i: Int)(using ClassTag[A]): NArray[A] =
      m(::, i).raw
    end col

  end extension
end matrixUtil
