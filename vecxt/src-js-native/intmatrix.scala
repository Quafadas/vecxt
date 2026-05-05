package vecxt

import vecxt.BoundsCheck
import vecxt.dimensionExtender.DimensionExtender.*
import vecxt.matrix.Matrix
import scala.annotation.targetName

object JvmIntMatrix:
  extension (m: Matrix[Int])

    private inline def reduceAlongDimension(
        dim: DimensionExtender,
        inline op: (Int, Int) => Int,
        inline initial: Int
    ): Matrix[Int] =
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

      Matrix[Int](newArr, newShape)(using BoundsCheck.DoBoundsCheck.no)
    end reduceAlongDimension

    @targetName("intMatrixMax")
    inline def max(dim: DimensionExtender): Matrix[Int] =
      reduceAlongDimension(dim, math.max, Int.MinValue)
    end max

    @targetName("intMatrixMin")
    inline def min(dim: DimensionExtender): Matrix[Int] =
      reduceAlongDimension(dim, math.min, Int.MaxValue)
    end min

    @targetName("intMatrixSum")
    inline def sum(dim: DimensionExtender): Matrix[Int] =
      reduceAlongDimension(dim, _ + _, 0)
    end sum

    @targetName("intMatrixProduct")
    inline def product(dim: DimensionExtender): Matrix[Int] =
      reduceAlongDimension(dim, _ * _, 1)
    end product

  end extension
end JvmIntMatrix
