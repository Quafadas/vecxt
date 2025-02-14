package vecxt

import scala.collection.mutable
import vecxt.matrix.Matrix
import vecxt.BoundsCheck.DoBoundsCheck.yes

import narr.*

object tensor:

  /** This is a sparse tensor
    *
    * ._1 is the values, stored as a single contiguous array ._2 is the dimensions ._3 is the indices of the non-zero
    * elements.
    *
    * You really shouldn't use this, it's very much for my own curiosity.
    *
    * Might be threadsafe
    */
  opaque type SparseTensor = (mutable.ArrayBuffer[Double], Array[Int], mutable.ArrayBuffer[Int])

  object SparseTensor:

    def apply(values: mutable.ArrayBuffer[Double], dims: Array[Int], indices: mutable.ArrayBuffer[Int]): SparseTensor =
      require(values.length == indices.length, "Values and indices must have the same length")
      (values, dims, indices)
    end apply
  end SparseTensor

  extension (t: SparseTensor)

    inline def values: mutable.ArrayBuffer[Double] = t._1

    inline def dims: Array[Int] = t._2

    inline def indices: mutable.ArrayBuffer[Int] = t._3

    def update(index: Array[Int], value: Double): Unit = synchronized {
      val flatIndex = flattenIndex(index, t.dims)
      val idx = t.indices.indexOf(flatIndex)
      if idx >= 0 then t._1(idx) = value
      else
        t._1 += value
        t._3 += flatIndex
      end if
    }

    def apply(index: Array[Int]): Double = synchronized {
      val flatIndex = flattenIndex(index, t.dims)
      val idx = t.indices.indexOf(flatIndex)
      if idx >= 0 then t.values(idx) else 0.0
      end if
    }

    private def flattenIndex(index: Array[Int], dims: Array[Int]): Int =
      index.zip(dims).foldLeft(0) { case (acc, (i, dim)) => acc * dim + i }

    def clone(): SparseTensor = synchronized {
      val newValues = t.values.clone()
      val newDims = t.dims.clone()
      val newIndices = t.indices.clone()
      (newValues, newDims, newIndices)
    }

    def multiply(matrix: Matrix[Double]): Matrix[Double] = synchronized {
      require(t.dims.length == 2, "Sparse tensor must be 2-dimensional for matrix multiplication")
      require(t.dims(1) == matrix.shape._1, "Matrix dimensions must align for multiplication")

      val resultRows = t.dims(0)
      val resultCols = matrix.shape._2
      val resultArray: NArray[Double] = NArray.fill[Double](matrix.raw.size)(0.0)

      for i <- 0 until t.values.length do
        val row = t.indices(i) / t.dims(1)
        val col = t.indices(i) % t.dims(1)
        val value = t.values(i)

        for j <- 0 until resultCols do resultArray(row * resultCols + j) += value * matrix.raw(col * resultCols + j)
        end for
      end for

      Matrix(resultArray, (resultRows, resultCols))
    }

  end extension

end tensor
