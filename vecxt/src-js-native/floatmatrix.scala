package vecxt

import vecxt.dimensionExtender.DimensionExtender.*
import vecxt.matrix.Matrix
import scala.annotation.targetName

object JvmFloatMatrix:
  extension (m: Matrix[Float])
    /** Reads every element through `m.layout.linearIndex`, which is just `offset + row * rowStride + col * colStride` —
      * valid for any layout, dense or strided, row-major or column-major. So unlike the element-wise SIMD ops in this
      * file (which need a simple contiguous layout to hand `m.raw` to a vectorized loop), this needs no
      * `hasSimpleContiguousMemoryLayout` guard: there's no fast path to fall back from, just this one loop. `foreach2D`
      * picks whichever of row-major/column-major traversal order is cache-friendly for `m`'s own layout; which order it
      * picks doesn't matter for correctness here since `op` (max/min/sum/product) is always commutative and
      * associative.
      */
    private inline def reduceAlongDimension(
        dim: DimensionExtender,
        inline op: (Float, Float) => Float,
        inline initial: Float
    ): Matrix[Float] =
      val whichDim = dim.asInt
      val newShape = m.shape match
        case (r, c) if whichDim == 0 => (r, 1)
        case (r, c) if whichDim == 1 => (1, c)
        case _                       => ???

      val newArr = Array.fill(newShape._1 * newShape._2)(initial)
      m.layout.foreach2D { (i, j) =>
        val idx = m.layout.linearIndex(i, j)
        if whichDim == 0 then newArr(i) = op(newArr(i), m.raw(idx))
        end if
        if whichDim == 1 then newArr(j) = op(newArr(j), m.raw(idx))
        end if
      }

      Matrix[Float](newArr, newShape)
    end reduceAlongDimension

    @targetName("floatMatrixMax")
    def max(dim: DimensionExtender): Matrix[Float] =
      reduceAlongDimension(dim, math.max, Float.MinValue)
    end max

    @targetName("floatMatrixMin")
    def min(dim: DimensionExtender): Matrix[Float] =
      reduceAlongDimension(dim, math.min, Float.MaxValue)
    end min

    @targetName("floatMatrixSum")
    def sum(dim: DimensionExtender): Matrix[Float] =
      reduceAlongDimension(dim, _ + _, 0.0f)
    end sum

    @targetName("floatMatrixProduct")
    def product(dim: DimensionExtender): Matrix[Float] =
      reduceAlongDimension(dim, _ * _, 1.0f)
    end product

  end extension
end JvmFloatMatrix
