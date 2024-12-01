package vecxt

import scala.reflect.ClassTag

import vecxt.BoundsCheck.BoundsCheck
import vecxt.MatrixInstance.*
import vecxt.arrayUtil.printArr
import vecxt.matrix.*

import narr.*
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
        val (row, col) = m.tupleFromIdx(idx)
        newMat((row, col)) = m((col, row))

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
          val aRow = m.row(i)
          val els =
            for (el <- aRow)
              yield el.toString
          els.mkString(" ")
      end arrArr
      arrArr.mkString("\n")
    end printMat

    inline def col(i: Int)(using ClassTag[A]): NArray[A] =
      m(::, i).raw
    end col

  end extension
end matrixUtil
