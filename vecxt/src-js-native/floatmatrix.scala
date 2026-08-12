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

    /** Writes `alpha * (m @@ vec) + beta * y` into `y` in place. JS/Native counterpart of `JvmFloatMatrix.*=` on the
      * JVM, giving the operation the same signature and the same semantics on every platform.
      *
      * Elementwise rather than BLAS-backed, unlike every other platform's matrix-vector product, and that is a
      * limitation of where this file sits rather than a judgement that the loop is preferable. `src-js-native` is
      * compiled into both JS and Native, so it can only contain code valid for both — and their BLAS shims are
      * different libraries (`@stdlib/blas` versus `org.ekrich.blas`'s CBLAS). Reaching either would mean splitting this
      * file per platform, or adding `JsFloatMatrix`/`NativeFloatMatrix` objects to mirror how `Double` is arranged,
      * neither of which is worth doing silently.
      *
      * The loop is correct for every layout, which the BLAS paths on other platforms are not without their guards, so
      * this is slower on Native rather than wrong anywhere. On JS it is very likely faster than the shim would be:
      * `dgemv` there marshals the whole backing array into a `Float64Array` and the result back out, which is the same
      * order of work as the product itself.
      *
      * Matches the JVM's `beta == 0` handling, where the destination is written without being read.
      *
      * `alpha` and `beta` carry no defaults, matching the JVM signature: `all` exports both element types into one
      * scope and Scala allows only one overload of a name to have default arguments, which the `Double` twin already
      * uses. `matmulInPlace!` splits the same way for the same reason.
      *
      * @param vec
      *   the vector to multiply by; must have length `m.cols`
      * @param y
      *   the destination, accumulated onto per `beta`; must have length `m.rows`
      */
    def *=(vec: Array[Float], y: Array[Float], alpha: Float, beta: Float): Unit =
      if vec.length != m.cols then
        throw new IllegalArgumentException(s"Vector length ${vec.length} != expected ${m.cols}")
      end if
      if y.length != m.rows then
        throw new IllegalArgumentException(s"Destination length ${y.length} != expected ${m.rows}")
      end if
      var i = 0
      while i < m.rows do
        var acc = 0.0f
        var j = 0
        while j < m.cols do
          acc += m.raw(m.layout.linearIndex(i, j)) * vec(j)
          j += 1
        end while
        y(i) = if beta == 0.0f then alpha * acc else alpha * acc + beta * y(i)
        i += 1
      end while
    end *=

    /** Matrix-vector product: returns `m @@ vec` as a fresh array. Wrapper over [[*=]] with `beta = 0`, so the
      * freshly allocated destination is written without being read. Two arities rather than a defaulted `alpha`, for
      * the reason given on [[*=]].
      */
    @targetName("matmulFloatVector")
    def *(vec: Array[Float]): Array[Float] = m.*(vec, 1.0f)

    @targetName("matmulFloatVectorScaled")
    def *(vec: Array[Float], alpha: Float): Array[Float] =
      val out = Array.ofDim[Float](m.rows)
      m.*=(vec, out, alpha, 0.0f)
      out
    end *

  end extension
end JvmFloatMatrix
