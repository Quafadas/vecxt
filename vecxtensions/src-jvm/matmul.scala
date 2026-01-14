package vecxtensions

import scala.reflect.ClassTag

import vecxt.*
import vecxt.BoundsCheck.BoundsCheck
import vecxt.all.*


import spire.algebra.Ring
import spire.implicits.*

object SpireExt:

  extension [A: ClassTag: Ring](m1: Matrix[A])
    inline def @@@(
        m2: Matrix[A]
    )(using inline boundsCheck: BoundsCheck): Matrix[A] =
      dimMatCheck(m1, m2)
      val (r1, c1) = m1.shape
      val (r2, c2) = m2.shape

      val nar = Array.ofDim[A](r1 * c2)
      val res = Matrix(nar, (r1, c2))

      for i <- 0 until r1 do
        for j <- 0 until c2 do
          res((i, j)) = (0 until c1)
            .map { k =>
              val i1 = m1((i: Row, k: Col))
              val i2 = m2((k: Row, j: Col))
              i1 * i2
            }
            .reduce(_ + _)
      end for
      res
    end @@@

    inline def showMat: String =
      val (r, c) = m1.shape
      val sb = new StringBuilder
      for i <- 0 until r do
        for j <- 0 until c do
          sb.append(m1((i: Row, j: Col))(using BoundsCheck.DoBoundsCheck.no))
          sb.append(" ")
        end for
        sb.append("\n")
      end for
      sb.toString
    end showMat
  end extension
end SpireExt
