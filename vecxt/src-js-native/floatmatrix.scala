package vecxt

import vecxt.BoundsCheck
import vecxt.dimensionExtender.DimensionExtender.*
import vecxt.matrix.Matrix
import scala.annotation.targetName

object JvmFloatMatrix:
  extension (m: Matrix[Float])
    /** Returns the sum of each column as a flat Array[Float].
      *
      * For dense column-major matrices iterates directly on the backing array at each column's base offset — no
      * intermediate array is allocated per column.
      */
    inline def colSums: Array[Float] =
      val result = Array.ofDim[Float](m.cols)
      var i = 0
      if m.isDenseColMajor then
        while i < m.cols do
          val colBase = i * m.rows
          var j = 0
          var acc = 0.0f
          while j < m.rows do
            acc += m.raw(colBase + j)
            j += 1
          end while
          result(i) = acc
          i += 1
        end while
      else
        while i < m.cols do
          var acc = 0.0f
          var j = 0
          while j < m.rows do
            acc += m.raw(m.offset + j * m.rowStride + i * m.colStride)
            j += 1
          end while
          result(i) = acc
          i += 1
        end while
      end if
      result
    end colSums

    private inline def reduceAlongDimension(
        dim: DimensionExtender,
        inline op: (Float, Float) => Float,
        inline initial: Float
    ): Matrix[Float] =
      if !m.hasSimpleContiguousMemoryLayout then ???
      end if
      val whichDim = dim.asInt
      val newShape = m.shape match
        case (r, c) if whichDim == 0 => (r, 1)
        case (r, c) if whichDim == 1 => (1, c)
        case _                       => ???

      val newArr = Array.fill(newShape._1 * newShape._2)(initial)
      var i = 0
      while i < m.cols do
        var j = 0
        while j < m.rows do
          val idx = i * m.rows + j
          if whichDim == 0 then newArr(j) = op(newArr(j), m.raw(idx))
          end if
          if whichDim == 1 then newArr(i) = op(newArr(i), m.raw(idx))
          end if
          j += 1
        end while
        i += 1
      end while

      Matrix[Float](newArr, newShape)(using BoundsCheck.DoBoundsCheck.no)
    end reduceAlongDimension

    @targetName("floatMatrixMax")
    inline def max(dim: DimensionExtender): Matrix[Float] =
      reduceAlongDimension(dim, math.max, Float.MinValue)
    end max

    @targetName("floatMatrixMin")
    inline def min(dim: DimensionExtender): Matrix[Float] =
      reduceAlongDimension(dim, math.min, Float.MaxValue)
    end min

    @targetName("floatMatrixSum")
    inline def sum(dim: DimensionExtender): Matrix[Float] =
      reduceAlongDimension(dim, _ + _, 0.0f)
    end sum

    @targetName("floatMatrixProduct")
    inline def product(dim: DimensionExtender): Matrix[Float] =
      reduceAlongDimension(dim, _ * _, 1.0f)
    end product

  end extension
end JvmFloatMatrix
