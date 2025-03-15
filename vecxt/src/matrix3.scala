package vecxt
import vecxt.BoundsCheck.BoundsCheck

import narr.*
import scala.reflect.ClassTag
import vecxt.matrix.Matrix
import vecxt.matrixUtil.col

import scala.annotation.targetName
import vecxt.arrayUtil.printArr

object matrix3:

  /** This is a matrix
    *
    * ._1 is the Matrix[A] values, stored as a single contiguous array ._2 is the dimensions ._2._1 is the number of
    * rows ._2._2 is the number of columns. You can access the raw array with the .raw method which inlines to the tuple
    * call.
    *
    * Storage is column major.
    */

  opaque type Matrix3[@specialized(Double, Boolean, Int) A] = (NArray[A], RowColDepth)

  object Matrix3:

    // inline def apply(raw: narr.native.DoubleArray, dim: RowCol)(using
    //     inline boundsCheck: BoundsCheck
    // ): Matrix[Double] =
    //   dimMatDInstantiateCheck(raw, dim)
    //   (raw, dim)
    // end apply

    inline def apply[@specialized(Double, Boolean, Int) A](raw: NArray[A], dim: RowColDepth)(using
        inline boundsCheck: BoundsCheck
    ): Matrix3[A] =
      dimMatInstantiateCheck(raw, dim)
      (raw, dim)
    end apply
    inline def apply[@specialized(Double, Boolean, Int) A](dim: RowColDepth, raw: NArray[A])(using
        inline boundsCheck: BoundsCheck
    ): Matrix3[A] =
      dimMatInstantiateCheck(raw, dim)
      (raw, dim)
    end apply

    inline def fromDepthMatricies[@specialized(Double, Boolean, Int) A: ClassTag](
        depthMatrices: Matrix[A]*
    )(using inline boundsCheck: BoundsCheck): Matrix3[A] =
      val depth = depthMatrices.length
      val rows = depthMatrices.head.shape._1
      val cols = depthMatrices.head.shape._2
      val rawData = NArray.ofSize[A](rows * cols * depth)

      val step = rows * cols
      for i <- 0 until depth do
        val matThisDepth = depthMatrices(i)
        val idx = i * step
        matThisDepth.raw.copyToNArray(rawData, idx, step)
      end for

      Matrix3(rawData, (rows, cols, depth))
    end fromDepthMatricies

    inline def fromRowMatrices[@specialized(Double, Boolean, Int) A: ClassTag](
        rowMatrices: Matrix[A]*
    )(using inline boundsCheck: BoundsCheck): Matrix3[A] =
      val depth = rowMatrices.head.shape._1
      val rows = rowMatrices.length
      val cols = rowMatrices.head.shape._2

      val rawData = NArray.ofSize[A](rows * cols * depth)

      for i <- 0 until rows do
        val thismat = rowMatrices(i)
        for j <- 0 until cols do
          for k <- 0 until depth do
            val idx = k * rows * cols + j * rows + i
            rawData(idx) = thismat.raw(j * depth + k)
          end for
        end for
      end for

      Matrix3(rawData, (rows, cols, depth))
    end fromRowMatrices

    inline def fromColumnMatrices[@specialized(Double, Boolean, Int) A: ClassTag](
        columnMatrices: Matrix[A]*
    )(using inline boundsCheck: BoundsCheck): Matrix3[A] =
      val depth = columnMatrices.head.shape._2
      val rows = columnMatrices.head.shape._1
      val cols = columnMatrices.length
      val rawData = NArray.ofSize[A](rows * cols * depth)

      for i <- 0 until cols do
        val mat = columnMatrices(i)
        for j <- 0 until depth do
          val thisCol = mat.col(j)
          val idx = j * rows * cols + i * rows
          thisCol.copyToNArray(rawData, idx, thisCol.length)
        end for
      end for
      Matrix3(rawData, (rows, cols, depth))
    end fromColumnMatrices

    inline def transpose[@specialized(Double, Boolean, Int) A: ClassTag](matrix: Matrix3[A]): Matrix3[A] =
      val (raw, (rows, cols, depth)) = matrix
      val transposedData = NArray.ofSize[A](raw.length)

      for i <- 0 until rows do
        for j <- 0 until cols do
          for k <- 0 until depth do
            val srcIdx = i * cols * depth + j * depth + k
            val destIdx = j * rows * depth + i * depth + k
            transposedData(destIdx) = raw(srcIdx)
          end for
        end for
      end for

      Matrix3(transposedData, (cols, rows, depth))(using BoundsCheck.DoBoundsCheck.no)
    end transpose

  end Matrix3

  extension [@specialized(Double, Boolean, Int) A](m: Matrix3[A])

    @targetName("mat3raw")
    inline def raw: NArray[A] = m._1

    inline def shape: RowColDepth = m._2

  end extension

end matrix3
