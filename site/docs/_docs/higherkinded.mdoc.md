---
title: Matrix[A]
---

Vecxt matrix is a higher kinded thing with no bounds. Vecxt tries to squeeze the best performance out of `Double`, but doesn't try to impose restrictions on what you can do with it.

Here we offer a matrix multiplication extension method based on Spires typeclasses.

```scala

object SpireExt:

  extension [A: ClassTag: Ring](m1: Matrix[A])
    inline def @@@(
        m2: Matrix[A]
    )(using inline boundsCheck: BoundsCheck): Matrix[A] =
      dimMatCheck(m1, m2)
      val (r1, c1) = m1.shape
      val (r2, c2) = m2.shape

      val nar = NArray.ofSize[A](r1 * c2)
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


val mat1 = Matrix.fromRows[Complex[Double]](
  NArray[Complex[Double]](Complex(1.0, -1.0), Complex(0.0, 2.0), Complex(-2.0, 1.0)),
  NArray[Complex[Double]](Complex(0.0, -3.0), Complex(3.0, -2.0), Complex(-1.0, -1.0))
)

val mat2 = Matrix.fromRows[Complex[Double]](
  NArray[Complex[Double]](Complex(0.0, -2.0), Complex(1.0, -4.0)),
  NArray[Complex[Double]](Complex(-1.0, 3.0), Complex(2.0, -3.0)),
  NArray[Complex[Double]](Complex(-2.0, 1.0), Complex(-4.0, 1.0))
)

import SpireExt.*

mat1 @@@ mat2

```



