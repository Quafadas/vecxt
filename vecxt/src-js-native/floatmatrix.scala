package vecxt

import vecxt.all.*
import vecxt.dimensionExtender.DimensionExtender.*
import vecxt.matrix.Matrix
import scala.annotation.targetName

object JvmFloatMatrix:
  extension (m: Matrix[Float])
    /** `Boolean`-masked elementwise multiply: keeps `m(i, j)` where `bmat(i, j)` is `true`, zeroes it otherwise. Float
      * counterpart of `JvmFloatMatrix.*:*`, minus the SIMD fast path in `*:*=` below (no `jdk.incubator.vector` on
      * JS/Native) — both branches here are plain `foreach2D`/array loops instead.
      */
    @targetName("matmulFloatElementWise")
    def *:*(bmat: Matrix[Boolean]): Matrix[Float] =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        val copy = m.deepCopy(asRowMajor = m.isDenseRowMajor)
        copy *:*= bmat
        copy
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val bIdx = bmat.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = if bmat.raw(bIdx) then m.raw(mIdx) else 0.0f
        }
        Matrix[Float](newArr, m.rows, m.cols)
      end if
    end *:*

    @targetName("matmulFloatElementWiseInPlace")
    def *:*=(bmat: Matrix[Boolean]): Unit =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        var i = 0
        while i < m.raw.length do
          if !bmat.raw(i) then m.raw.update(i, 0.0f)
          end if
          i += 1
        end while
      else
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val bIdx = bmat.layout.linearIndex(i, j)
          if !bmat.raw(bIdx) then m.raw(mIdx) = 0.0f
          end if
        }
      end if
    end *:*=

    def >=(d: Float): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](vecxt.floatarrays.>=(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) >= d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    @targetName("floatmatrixGT")
    def >(d: Float): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](vecxt.floatarrays.>(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) > d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    @targetName("floatmatrixLE")
    def <=(d: Float): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](vecxt.floatarrays.<=(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) <= d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    @targetName("floatmatrixLT")
    def <(d: Float): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](vecxt.floatarrays.<(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) < d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    /** Adds the elements of `arr` to the matrix with broadcasting behavior: down each column if `rowStride == 1`,
      * across each row if `colStride == 1`, elementwise via `foreach2D` otherwise. No SIMD stride tricks here (unlike
      * `JvmFloatMatrix.+=`) — plain loops throughout, since there's no Vector API on JS/Native.
      */
    @targetName("floatmatrixAddVectorInPlace")
    def +=(arr: Array[Float]): Unit =
      assert(arr.length == m.cols, s"Array length ${arr.length} != expected ${m.cols}")
      if m.rowStride == 1 then
        var i = 0
        while i < m.cols do
          var j = 0
          val offsetI = m.offset + i * m.colStride
          while j < m.rows do
            val idx = offsetI + j
            m.raw(idx) = m.raw(idx) + arr(i)
            j += 1
          end while
          i += 1
        end while
      else if m.colStride == 1 then
        var j = 0
        while j < m.rows do
          var i = 0
          val offsetJ = m.offset + j * m.rowStride
          while i < m.cols do
            val idx = offsetJ + i
            m.raw(idx) = m.raw(idx) + arr(i)
            i += 1
          end while
          j += 1
        end while
      else
        m.layout.foreach2D { (i, j) =>
          m(i, j) = m(i, j) + arr(j)
        }
      end if
    end +=

    /** Elementwise `m - mat1`, the `Float` counterpart of `JvmFloatMatrix.-(mat1)`. Dense fast path delegates to
      * `vecxt.floatarrays.-`, which is cross-platform; the general fallback materialises a fresh dense row-major
      * result, matching the JVM version's layout policy.
      */
    @targetName("floatmatrixSubVector")
    def -(mat1: Matrix[Float]): Matrix[Float] =
      sameDimMatCheck(m, mat1)
      if sameDenseElementWiseMemoryLayoutCheck(m, mat1) then
        val newArr = vecxt.floatarrays.-(m.raw)(mat1.raw)
        Matrix(newArr, m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        val newMat = Matrix[Float](newArr, m.rows, m.cols, m.cols, 1, 0)
        m.layout.foreach2D { (i, j) =>
          newMat(i, j) = m(i, j) - mat1(i, j)
        }
        newMat
      end if
    end -

    /** Subtracts the elements of `arr` from the matrix with the same broadcasting behavior as [[+=]]. Float-only —
      * `DoubleMatrix` has no equivalent anywhere in the codebase — added here purely for Float's own cross-platform
      * consistency with `JvmFloatMatrix.-=(arr)`.
      */
    @targetName("floatmatrixSubVectorInPlace")
    def -=(arr: Array[Float]): Unit =
      assert(arr.length == m.cols, s"Array length ${arr.length} != expected ${m.cols}")
      if m.rowStride == 1 then
        var i = 0
        while i < m.cols do
          var j = 0
          val offsetI = m.offset + i * m.colStride
          while j < m.rows do
            val idx = offsetI + j
            m.raw(idx) = m.raw(idx) - arr(i)
            j += 1
          end while
          i += 1
        end while
      else if m.colStride == 1 then
        var j = 0
        while j < m.rows do
          var i = 0
          val offsetJ = m.offset + j * m.rowStride
          while i < m.cols do
            val idx = offsetJ + i
            m.raw(idx) = m.raw(idx) - arr(i)
            i += 1
          end while
          j += 1
        end while
      else
        m.layout.foreach2D { (i, j) =>
          m(i, j) = m(i, j) - arr(j)
        }
      end if
    end -=

    /** Scalar add in place. `JvmFloatMatrix.+=(n)` hand-rolls a stride-broadcast SIMD path for the non-contiguous case;
      * there's no Vector API here, so the non-contiguous fallback is plain `foreach2D` throughout.
      */
    @targetName("floatmatrixAddScalarInPlace")
    def +=(n: Float): Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.floatarrays.+=(m.raw)(n)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = m.raw(idx) + n
        }
      end if
    end +=

    @targetName("floatmatrixSubScalarInPlace")
    def -=(n: Float): Unit =
      if m.hasSimpleContiguousMemoryLayout then vecxt.floatarrays.-=(m.raw)(n)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = m.raw(idx) - n
        }
      end if
    end -=

    def *=(d: Float): Unit =
      if m.hasSimpleContiguousMemoryLayout then floatarrays.*=(m.raw)(d)
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = m.raw(idx) * d
        }
      end if
    end *=

    def *(d: Float): Matrix[Float] =
      val out = m.deepCopy
      out.*=(d)
      out
    end *

    def +(d: Float): Matrix[Float] =
      val out = m.deepCopy
      out.+=(d)
      out
    end +

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
        case _                       => throw InvalidDimensionException(whichDim)

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

  /** Scalar-left operators, the `Float` counterpart of `DoubleMatrix`'s `extension (d: Double)`. `*` / `+` / `*=` /
    * `+=` are commutative, so they delegate to the scalar-right forms on `Matrix[Float]` above; `-` / `/` / `-=` / `/=`
    * are not, so each needs its own body computing `d op m(i, j)` rather than `m(i, j) op d`.
    *
    * Layout policy for the allocating `-` / `/`: identical to `Matrix[Float]#*(d: Float)` and to the `Double`
    * equivalents — the dense-contiguous fast path wraps the transformed array with `m.layout`, so the result keeps
    * `m`'s own orientation, and the general path materialises row-major when `m`'s unit-stride axis is columns and
    * column-major otherwise (including when `m` has no unit-stride axis at all).
    */
  extension (d: Float)
    def *(m: Matrix[Float]): Matrix[Float] = m * d

    def +(m: Matrix[Float]): Matrix[Float] = m + d

    /** Elementwise `d - m(i, j)`. Not `m - d`: subtraction isn't commutative, so this can't delegate like `*` / `+`. */
    def -(m: Matrix[Float]): Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.floatarrays.-(d)(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        val asRowMajor = m.layout.unitStrideAxis == 1
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(if asRowMajor then i * m.cols + j else i + j * m.rows) = d - m.raw(srcIdx)
        }
        if asRowMajor then Matrix[Float](newArr, m.rows, m.cols, m.cols, 1, 0)
        else Matrix[Float](newArr, m.rows, m.cols, 1, m.rows, 0)
        end if
      end if
    end -

    /** Elementwise `d / m(i, j)`. Not `m / d`: division isn't commutative either. */
    def /(m: Matrix[Float]): Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.floatarrays./(d)(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        val asRowMajor = m.layout.unitStrideAxis == 1
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(if asRowMajor then i * m.cols + j else i + j * m.rows) = d / m.raw(srcIdx)
        }
        if asRowMajor then Matrix[Float](newArr, m.rows, m.cols, m.cols, 1, 0)
        else Matrix[Float](newArr, m.rows, m.cols, 1, m.rows, 0)
        end if
      end if
    end /

    def *=(m: Matrix[Float]): Unit = m *= d

    /** `d += m` delegates: `Matrix[Float]#+=(n: Float)` above is already the broadcast-in-place implementation, and
      * addition is commutative, so there is nothing scalar-left-specific to do.
      */
    def +=(m: Matrix[Float]): Unit = m += d

    /** `d -= m` / `d /= m` overwrite `m` in place with `d - m(i, j)` / `d / m(i, j)`. They cannot delegate to `m -= d`
      * / `m /= d` (and `Matrix[Float]` has no `/=(n: Float)` to delegate to in any case). Dense contiguous layouts walk
      * `m.raw` straight through; anything else goes element-by-element via `linearIndex`, which skips padding and
      * honours arbitrary strides and offsets.
      */
    def -=(m: Matrix[Float]): Unit =
      if m.hasSimpleContiguousMemoryLayout then
        var i = 0
        while i < m.raw.length do
          m.raw(i) = d - m.raw(i)
          i += 1
        end while
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = d - m.raw(idx)
        }
      end if
    end -=

    def /=(m: Matrix[Float]): Unit =
      if m.hasSimpleContiguousMemoryLayout then
        var i = 0
        while i < m.raw.length do
          m.raw(i) = d / m.raw(i)
          i += 1
        end while
      else
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = d / m.raw(idx)
        }
      end if
    end /=

  end extension
end JvmFloatMatrix
