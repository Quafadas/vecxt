package vecxt

import vecxt.dimensionExtender.DimensionExtender.*
import vecxt.matrix.Matrix
import scala.annotation.targetName

object JvmIntMatrix:
  extension (m: Matrix[Int])

    /** Reads every element through `m.layout.linearIndex`, which is just `offset + row * rowStride + col * colStride` —
      * valid for any layout, dense or strided, row-major or column-major. So unlike the element-wise SIMD ops in this
      * file (which need a simple contiguous layout to hand `m.raw` to a vectorized loop), this needs no
      * `hasSimpleContiguousMemoryLayout` guard: there's no fast path to fall back from, just this one loop.
      */
    private inline def reduceAlongDimension(
        dim: DimensionExtender,
        inline op: (Int, Int) => Int,
        inline initial: Int
    ): Matrix[Int] =
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
          val idx = m.layout.linearIndex(j, i)
          if whichDim == 0 then newArr(j) = op(newArr(j), m.raw(idx))
          end if
          if whichDim == 1 then newArr(i) = op(newArr(i), m.raw(idx))
          end if
          j += 1
        end while
        i += 1
      end while

      Matrix[Int](newArr, newShape)
    end reduceAlongDimension

    @targetName("intMatrixMax")
    def max(dim: DimensionExtender): Matrix[Int] =
      reduceAlongDimension(dim, math.max, Int.MinValue)
    end max

    @targetName("intMatrixMin")
    def min(dim: DimensionExtender): Matrix[Int] =
      reduceAlongDimension(dim, math.min, Int.MaxValue)
    end min

    @targetName("intMatrixSum")
    def sum(dim: DimensionExtender): Matrix[Int] =
      reduceAlongDimension(dim, _ + _, 0)
    end sum

    @targetName("intMatrixProduct")
    def product(dim: DimensionExtender): Matrix[Int] =
      reduceAlongDimension(dim, _ * _, 1)
    end product

  end extension
end JvmIntMatrix
