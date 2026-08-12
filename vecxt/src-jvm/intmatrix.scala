package vecxt

import vecxt.dimensionExtender.DimensionExtender.*
import vecxt.intarrays.*
import vecxt.matrix.*
import vecxt.MatrixInstance.*
import jdk.incubator.vector.IntVector
import jdk.incubator.vector.VectorMask
import scala.annotation.targetName

object JvmIntMatrix:
  extension (m: Matrix[Int])

    inline def matmul(b: Matrix[Int]): Matrix[Int] =
      dimMatCheck(m, b)
      scala.compiletime.error(
        "Integer matrix multiplication is not implemented yet. Use Float or Double matrices instead."
      )

    end matmul

    // The six scalar ops below share one shape, mirroring `JvmFloatMatrix`'s equivalents: a whole-array SIMD fast path
    // when the backing array already holds exactly the elements in order, and otherwise an elementwise `foreach2D`
    // read through `linearIndex`, which is valid for any offset/stride.
    //
    // Note the two branches disagree on the result's storage order, exactly as the Float versions do: the fast path
    // reuses `m.layout` (so a dense row-major input yields a row-major result), while the elementwise branch
    // normalises to column-major. Both describe the same logical matrix — `foreach2D` visits every (i, j) once and
    // the destination index is computed explicitly — so this is a storage difference, not a correctness one.

    @scala.annotation.targetName("intMatrixDivDouble")
    def /(d: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Double](vecxt.intarrays./(i)(d), m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) / d
        }
        Matrix[Double](newArr, m.rows, m.cols)
    end /

    @scala.annotation.targetName("intMatrixDivFloat")
    def /(d: Float): Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Float](vecxt.intarrays./(i)(d), m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) / d
        }
        Matrix[Float](newArr, m.rows, m.cols)
    end /

    def >=(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.gte(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) >= d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)
    end >=

    def >(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.gt(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) > d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)
    end >

    def <=(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.lte(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) <= d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)
    end <=

    def <(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.lt(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) < d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)
    end <

    @scala.annotation.targetName("intMatrixMaskInPlace")
    def *:*=(bmat: Matrix[Boolean]): Unit =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        val spi = IntVector.SPECIES_PREFERRED
        val spil = spi.length()
        val zero = IntVector.zero(spi)
        var i = 0
        while i < spi.loopBound(m.raw.length) do
          val mask = VectorMask.fromArray(spi, bmat.raw, i)
          zero.blend(IntVector.fromArray(spi, m.raw, i), mask).intoArray(m.raw, i)
          i += spil
        end while
        while i < m.raw.length do
          if !bmat.raw(i) then m.raw.update(i, 0)
          end if
          i += 1
        end while
      else
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val bIdx = bmat.layout.linearIndex(i, j)
          if !bmat.raw(bIdx) then m.raw(mIdx) = 0
          end if
        }
      end if
    end *:*=

    @scala.annotation.targetName("intMatrixMask")
    def *:*(bmat: Matrix[Boolean]): Matrix[Int] =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        // Preserve m's own orientation: deepCopy defaults to column-major, which would mismatch bmat's
        // orientation whenever m (and hence bmat, per the check above) is dense row-major, sending the
        // `*:*=` below into its slower foreach2D fallback instead of the fast path both were just confirmed to share.
        val copy = m.deepCopy(asRowMajor = m.isDenseRowMajor)
        copy *:*= bmat
        copy
      else
        val newArr = Array.ofDim[Int](m.numel)
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val bIdx = bmat.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = if bmat.raw(bIdx) then m.raw(mIdx) else 0
        }
        Matrix[Int](newArr, m.rows, m.cols)
      end if
    end *:*

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
        inline op: (Int, Int) => Int,
        inline initial: Int
    ): Matrix[Int] =
      val whichDim = dim.asInt
      val newShape = m.shape match
        case (r, c) if whichDim == 0 => (r, 1)
        case (r, c) if whichDim == 1 => (1, c)
        case _                       => throw InvalidDimensionException(whichDim)

      val newArr = Array.fill(newShape._1 * newShape._2)(initial)
      m.layout.foreach2D { (i, j) =>
        val idx = m.layout.linearIndex(i, j)
        if whichDim == 0 then newArr(i) = op(newArr(i), m.raw(idx))
        end if
        if whichDim == 1 then newArr(j) = op(newArr(j), m.raw(idx))
        end if
      }

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

object NativeIntMatrix:

end NativeIntMatrix
