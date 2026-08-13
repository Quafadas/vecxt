package vecxt

import vecxt.MatrixInstance.*
import vecxt.dimensionExtender.DimensionExtender.*
import vecxt.floatarrays.*
import vecxt.all.`matmulInPlace!`
import vecxt.all.`matmul`
import vecxt.matrix.*
import vecxt.matrixUtil.*

object FloatMatrix:

  // extension (d: Double)
  //   def *(m: Matrix[Double]): Matrix[Double] = m * d
  //   def +(m: Matrix[Double]): Matrix[Double] = m + d

  //   /** Elementwise `d - m(i, j)`. Not `m - d` (that's `Matrix[Double]#-(n: Double)`) - subtraction isn't commutative,
  //     * so this needs its own body rather than delegating like `*`/`+` above. Layout policy: see
  //     * `Matrix[Double]#*(n: Double)`.
  //     */
  //   def -(m: Matrix[Double]): Matrix[Double] =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.doublearrays.-(d)(m.raw), m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       val asRowMajor = m.layout.unitStrideAxis == 1
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(if asRowMajor then i * m.cols + j else i + j * m.rows) = d - m.raw(srcIdx)
  //       }
  //       if asRowMajor then Matrix[Double](newArr, m.rows, m.cols, m.cols, 1, 0)
  //       else Matrix[Double](newArr, m.rows, m.cols, 1, m.rows, 0)
  //       end if
  //     end if
  //   end -

  //   /** Elementwise `d / m(i, j)`. Not `m / d` (that's `Matrix[Double]#/(n: Double)`) - division isn't commutative
  //     * either. Layout policy: see `Matrix[Double]#*(n: Double)`.
  //     */
  //   def /(m: Matrix[Double]): Matrix[Double] =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.doublearrays./(d)(m.raw), m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       val asRowMajor = m.layout.unitStrideAxis == 1
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(if asRowMajor then i * m.cols + j else i + j * m.rows) = d / m.raw(srcIdx)
  //       }
  //       if asRowMajor then Matrix[Double](newArr, m.rows, m.cols, m.cols, 1, 0)
  //       else Matrix[Double](newArr, m.rows, m.cols, 1, m.rows, 0)
  //       end if
  //     end if
  //   end /

  //   // `d *= m` and `d += m` can delegate (commutative scalar-left forms). `d -= m` and `d /= m` cannot:
  //   // they must apply true scalar-left semantics elementwise because subtraction/division are non-commutative.
  //   def *=(m: Matrix[Double]): Unit = m *= d
  //   def +=(m: Matrix[Double]): Unit =
  //     if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.+=(m.raw)(d)
  //     else
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         m.raw(idx) = m.raw(idx) + d
  //       }
  //     end if
  //   end +=
  //   // `d += m` is implemented directly above because the scalar-right `m += d` lives in platform files. In contrast,
  //   // `d -= m` / `d /= m` are non-commutative, so they must implement scalar-left semantics in place.
  //   def -=(m: Matrix[Double]): Unit =
  //     if m.hasSimpleContiguousMemoryLayout then
  //       var i = 0
  //       while i < m.raw.length do
  //         m.raw(i) = d - m.raw(i)
  //         i += 1
  //       end while
  //     else
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         m.raw(idx) = d - m.raw(idx)
  //       }
  //     end if
  //   end -=

  //   def /=(m: Matrix[Double]): Unit =
  //     if m.hasSimpleContiguousMemoryLayout then
  //       var i = 0
  //       while i < m.raw.length do
  //         m.raw(i) = d / m.raw(i)
  //         i += 1
  //       end while
  //     else
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         m.raw(idx) = d / m.raw(idx)
  //       }
  //     end if
  //   end /=

  // end extension

  extension (m: Matrix[Float])

    inline def @@(b: Matrix[Float]): Matrix[Float] =
      m.matmul(b, 1.0, 0.0)

  //   def *=(d: Double): Unit =
  //     if m.hasSimpleContiguousMemoryLayout then m.raw.multInPlace(d)
  //     else
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         m.raw(idx) = m.raw(idx) * d
  //       }

  //   /** In-place elementwise scalar subtract/divide. Same shape as `*=` above: SIMD fast path over the whole backing
  //     * array when `m` is dense contiguous, element-by-element via `linearIndex` otherwise - no result layout to pick
  //     * here (unlike `-`/`/`), since `m` keeps its own. (`+=(d: Double)` isn't defined here: each platform already has
  //     * its own more specialised stride-aware implementation - see e.g. `src-jvm/doublematrix.scala`.)
  //     */
  //   def -=(d: Double): Unit =
  //     if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.-=(m.raw)(d)
  //     else
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         m.raw(idx) = m.raw(idx) - d
  //       }

  //   def /=(d: Double): Unit =
  //     if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays./=(m.raw)(d)
  //     else
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         m.raw(idx) = m.raw(idx) / d
  //       }

  //   /** Elementwise scalar multiply.
  //     *
  //     * Layout policy (applies to `*`/`/`/`+`/`-` alike; see `site/docs/vectors-and-matrices/matrix.md`): the result is
  //     * row-major whenever `m`'s unit-stride axis is columns — true not only for a dense row-major `m`, but for any view
  //     * or padded layout that is still effectively row-major (`m.layout.unitStrideAxis == 1`) — and column-major
  //     * otherwise, including whenever `m` has no unit-stride axis at all. That's exactly what the fast path below
  //     * already does implicitly by wrapping the transformed array with `m.layout`; the non-dense branch used to always
  //     * normalise to column-major regardless, which made the result's layout depend on whether `m` happened to be
  //     * exactly dense rather than on `m`'s own orientation.
  //     */
  //   def *(n: Double): Matrix[Double] =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.doublearrays.*(m.raw)(n), m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       val asRowMajor = m.layout.unitStrideAxis == 1
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(if asRowMajor then i * m.cols + j else i + j * m.rows) = m.raw(srcIdx) * n
  //       }
  //       if asRowMajor then Matrix[Double](newArr, m.rows, m.cols, m.cols, 1, 0)
  //       else Matrix[Double](newArr, m.rows, m.cols, 1, m.rows, 0)
  //       end if
  //   end *

  //   /** Elementwise scalar divide. Layout policy: see `*`. */
  //   def /(n: Double): Matrix[Double] =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.doublearrays./(m.raw)(n), m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       val asRowMajor = m.layout.unitStrideAxis == 1
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(if asRowMajor then i * m.cols + j else i + j * m.rows) = m.raw(srcIdx) / n
  //       }
  //       if asRowMajor then Matrix[Double](newArr, m.rows, m.cols, m.cols, 1, 0)
  //       else Matrix[Double](newArr, m.rows, m.cols, 1, m.rows, 0)
  //       end if
  //   end /

  //   /** Elementwise scalar add. Layout policy: see `*`. */
  //   def +(n: Double): Matrix[Double] =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.doublearrays.+(m.raw)(n), m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       val asRowMajor = m.layout.unitStrideAxis == 1
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(if asRowMajor then i * m.cols + j else i + j * m.rows) = m.raw(srcIdx) + n
  //       }
  //       if asRowMajor then Matrix[Double](newArr, m.rows, m.cols, m.cols, 1, 0)
  //       else Matrix[Double](newArr, m.rows, m.cols, 1, m.rows, 0)
  //       end if
  //     end if

  //   end +

  //   def maximum(other: Matrix[Double]) =
  //     sameDimMatCheck(m, other)

  //     // TODO: SIMD optimization
  //     if sameDenseElementWiseMemoryLayoutCheck(m, other) then
  //       val newArr = Array.ofDim[Double](m.numel)
  //       var i = 0
  //       val bound = m.numel
  //       while i < bound do
  //         newArr(i) = math.max(m.raw(i), other.raw(i))
  //         i += 1
  //       end while
  //       // newArr is filled in m's own element order (row- or col-major), so it must be wrapped with m's
  //       // layout, not always assumed column-major — see `+:+` for the same pattern.
  //       Matrix(newArr, m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         val idxOther = other.layout.linearIndex(i, j)
  //         newArr(i + j * m.rows) = math.max(m.raw(idx), other.raw(idxOther))
  //       }
  //       Matrix[Double](newArr, m.rows, m.cols)
  //     end if
  //   end maximum

  //   /** Elementwise scalar subtract. Layout policy: see `*`. */
  //   def -(n: Double): Matrix[Double] =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix(vecxt.doublearrays.-(m.raw)(n), m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       val asRowMajor = m.layout.unitStrideAxis == 1
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(if asRowMajor then i * m.cols + j else i + j * m.rows) = m.raw(srcIdx) - n
  //       }
  //       if asRowMajor then Matrix[Double](newArr, m.rows, m.cols, m.cols, 1, 0)
  //       else Matrix[Double](newArr, m.rows, m.cols, 1, m.rows, 0)
  //       end if
  //   end -    

    def +:+(m2: Matrix[Float]): Matrix[Float] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.floatarrays.+(m.raw)(m2.raw)
        Matrix(newArr, m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val m2Idx = m2.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(mIdx) + m2.raw(m2Idx)
        }
        Matrix[Float](newArr, m.rows, m.cols)
      end if
    end +:+

    inline def +(m2: Matrix[Float]): Matrix[Float] = m +:+ m2

  //   def *(m2: Matrix[Double]): Matrix[Double] = m.hadamard(m2)

  //   def kronecker(other: Matrix[Double]): Matrix[Double] = ???

  //   def hadamard(m2: Matrix[Double]): Matrix[Double] =
  //     sameDimMatCheck(m, m2)

  //     if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
  //       // Fast path: use SIMD-optimized array multiplication
  //       val newArr = vecxt.doublearrays.*(m.raw)(m2.raw)
  //       Matrix(newArr, m.layout)
  //     else
  //       // Different memory layouts: materialize one matrix to match the other's layout.
  //       //
  //       // Each branch below multiplies the "already dense" side's `.raw` directly (via `dimCheck`, which requires
  //       // exact array-length equality), against a fresh `numel`-sized deepCopy of the other side. `isDenseColMajor`
  //       // / `isDenseRowMajor` alone do not guarantee `raw.length == numel` — a `submatrix` view of the leading
  //       // columns/rows of a wider/taller parent (`layoutCorpus.test.scala`'s `ColMajorLeadingCols` /
  //       // `RowMajorLeadingRows`) is dense by that narrower definition but keeps the parent's full backing array. So
  //       // each guard here additionally requires `hasSimpleContiguousMemoryLayout`, which folds in
  //       // `dataLength == numel`; a dense-but-padded operand instead falls through to the fully-general last branch
  //       // below, which deep-copies both sides and therefore never has a length mismatch.
  //       if m.hasSimpleContiguousMemoryLayout && m.isDenseColMajor then
  //         val m2Dense = m2.deepCopy(asRowMajor = false)
  //         vecxt.doublearrays.*:*=(m2Dense.raw)(m.raw)
  //         m2Dense
  //       else if m.hasSimpleContiguousMemoryLayout && m.isDenseRowMajor then
  //         // m is dense row-major, materialize m2 to row-major and multiply in-place
  //         val m2Dense = m2.deepCopy(asRowMajor = true)
  //         vecxt.doublearrays.*=(m2Dense.raw)(m.raw)
  //         m2Dense
  //       else if m2.hasSimpleContiguousMemoryLayout && m2.isDenseColMajor then
  //         // m2 is dense column-major, materialize m to column-major and multiply in-place
  //         val mDense = m.deepCopy(asRowMajor = false)
  //         vecxt.doublearrays.*=(mDense.raw)(m2.raw)
  //         mDense
  //       else if m2.hasSimpleContiguousMemoryLayout && m2.isDenseRowMajor then
  //         // m2 is dense row-major, materialize m to row-major and multiply in-place
  //         val mDense = m.deepCopy(asRowMajor = true)
  //         vecxt.doublearrays.*=(mDense.raw)(m2.raw)
  //         mDense
  //       else
  //         // Neither is dense, materialize both to column-major and use SIMD multiplication
  //         val mDense = m.deepCopy(asRowMajor = false)
  //         val m2Dense = m2.deepCopy(asRowMajor = false)
  //         val newArr = vecxt.doublearrays.*(mDense.raw)(m2Dense.raw)
  //         Matrix[Double](newArr, m.rows, m.cols)
  //       end if
  //     end if
  //   end hadamard

  //   def /:/(m2: Matrix[Double]): Matrix[Double] =
  //     sameDimMatCheck(m, m2)
  //     if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
  //       val newArr = vecxt.doublearrays./(m.raw)(m2.raw)
  //       Matrix(newArr, m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       m.layout.foreach2D { (i, j) =>
  //         val mIdx = m.layout.linearIndex(i, j)
  //         val m2Idx = m2.layout.linearIndex(i, j)
  //         newArr(i + j * m.rows) = m.raw(mIdx) / m2.raw(m2Idx)
  //       }
  //       Matrix[Double](newArr, m.rows, m.cols)
  //     end if
  //   end /:/

    // TODO: SIMD on JVM
    def -:-(m2: Matrix[Float]): Matrix[Float] =
      sameDimMatCheck(m, m2)
      if sameDenseElementWiseMemoryLayoutCheck(m, m2) then
        val newArr = vecxt.floatarrays.-(m.raw)(m2.raw)
        Matrix(newArr, m.layout)
      else
        val newArr = Array.ofDim[Float](m.numel)
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val m2Idx = m2.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(mIdx) - m2.raw(m2Idx)
        }
        Matrix[Float](newArr, m.rows, m.cols)
      end if
    end -:-

    def -(m2: Matrix[Float]): Matrix[Float] = m -:- m2

  //   def unary_- : Matrix[Double] =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.doublearrays.unary_-(m.raw), m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(i + j * m.rows) = -m.raw(srcIdx)
  //       }
  //       Matrix[Double](newArr, m.rows, m.cols)

  //   def `exp!`: Unit =
  //     if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`exp!`(m.raw)
  //     else
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         m.raw(idx) = Math.exp(m.raw(idx))
  //       }

  //   def `log!`: Unit =
  //     if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`log!`(m.raw)
  //     else
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         m.raw(idx) = Math.log(m.raw(idx))
  //       }

  //   def exp: Matrix[Double] =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.exp(m.raw), m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(i + j * m.rows) = Math.exp(m.raw(srcIdx))
  //       }
  //       Matrix[Double](newArr, m.rows, m.cols)

  //   def log: Matrix[Double] =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.log(m.raw), m.layout)
  //     else
  //       // allocate a fresh column-major matrix (rowStride=1, colStride=rows)
  //       val newArr = Array.ofDim[Double](m.numel)
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(i + j * m.rows) = Math.log(m.raw(srcIdx))
  //       }
  //       Matrix[Double](newArr, m.rows, m.cols)

  //   def `sqrt!`: Unit =
  //     if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`sqrt!`(m.raw)
  //     else
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         m.raw(idx) = Math.sqrt(m.raw(idx))
  //       }
  //     end if
  //   end `sqrt!`

  //   def sqrt: Matrix[Double] =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.sqrt(m.raw), m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(i + j * m.rows) = Math.sqrt(m.raw(srcIdx))
  //       }
  //       Matrix[Double](newArr, m.rows, m.cols)

  //   def sin =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.sin(m.raw), m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(i + j * m.rows) = Math.sin(m.raw(srcIdx))
  //       }
  //       Matrix[Double](newArr, m.rows, m.cols)

  //   def `sin!` =
  //     if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`sin!`(m.raw)
  //     else
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         m.raw(idx) = Math.sin(m.raw(idx))
  //       }
  //     end if
  //   end `sin!`

  //   def cos =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.cos(m.raw), m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(i + j * m.rows) = Math.cos(m.raw(srcIdx))
  //       }
  //       Matrix[Double](newArr, m.rows, m.cols)

  //   def `cos!`: Unit =
  //     if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`cos!`(m.raw)
  //     else
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         m.raw(idx) = Math.cos(m.raw(idx))
  //       }
  //     end if
  //   end `cos!`

  //   def tan =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.tan(m.raw), m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(i + j * m.rows) = Math.tan(m.raw(srcIdx))
  //       }
  //       Matrix[Double](newArr, m.rows, m.cols)

  //   def `tan!` =
  //     if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.`tan!`(m.raw)
  //     else
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         m.raw(idx) = Math.tan(m.raw(idx))
  //       }
  //     end if
  //   end `tan!`

  //   def mean: Double =
  //     if m.hasSimpleContiguousMemoryLayout then m.sumSIMD / (m.rows * m.cols)
  //     else
  //       var acc = 0.0
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         acc += m.raw(idx)
  //       }
  //       acc / (m.rows * m.cols)
  //     end if
  //   end mean

  //   def **(power: Double): Matrix[Double] =
  //     if m.hasSimpleContiguousMemoryLayout then Matrix[Double](vecxt.all.**(m.raw)(power), m.layout)
  //     else
  //       val newArr = Array.ofDim[Double](m.numel)
  //       m.layout.foreach2D { (i, j) =>
  //         val srcIdx = m.layout.linearIndex(i, j)
  //         newArr(i + j * m.rows) = Math.pow(m.raw(srcIdx), power)
  //       }
  //       Matrix[Double](newArr, m.rows, m.cols)
  //     end if
  //   end **

  //   /** Reads every element through `m.layout.linearIndex`, which is just `offset + row * rowStride + col * colStride` —
  //     * valid for any layout, dense or strided, row-major or column-major. So unlike the element-wise SIMD ops in this
  //     * file (which need a simple contiguous layout to hand `m.raw` to a vectorized loop), this needs no
  //     * `hasSimpleContiguousMemoryLayout` guard: there's no fast path to fall back from, just this one loop. `foreach2D`
  //     * picks whichever of row-major/column-major traversal order is cache-friendly for `m`'s own layout; which order it
  //     * picks doesn't matter for correctness here since `op` (max/min/sum/product) is always commutative and
  //     * associative.
  //     */
  //   private inline def reduceAlongDimension(
  //       dim: DimensionExtender,
  //       inline op: (Double, Double) => Double,
  //       inline initial: Double
  //   ): Matrix[Double] =
  //     val whichDim = dim.asInt
  //     val newShape = m.shape match
  //       case (r, c) if whichDim == 0 => (r, 1)
  //       case (r, c) if whichDim == 1 => (1, c)
  //       case _                       => throw InvalidDimensionException(whichDim)

  //     val newArr = Array.fill(newShape._1 * newShape._2)(initial)
  //     m.layout.foreach2D { (i, j) =>
  //       val idx = m.layout.linearIndex(i, j)
  //       if whichDim == 0 then newArr(i) = op(newArr(i), m.raw(idx))
  //       end if
  //       if whichDim == 1 then newArr(j) = op(newArr(j), m.raw(idx))
  //       end if
  //     }

  //     Matrix[Double](newArr, newShape)
  //   end reduceAlongDimension

  //   def max(dim: DimensionExtender): Matrix[Double] =
  //     reduceAlongDimension(dim, math.max, Double.MinValue)
  //   end max

  //   def min(dim: DimensionExtender): Matrix[Double] =
  //     reduceAlongDimension(dim, math.min, Double.MaxValue)
  //   end min

  //   def sum(dim: DimensionExtender): Matrix[Double] =
  //     reduceAlongDimension(dim, _ + _, 0.0)
  //   end sum

  //   def product(dim: DimensionExtender): Matrix[Double] =
  //     reduceAlongDimension(dim, _ * _, 1.0)
  //   end product

  //   // inline def - : Matrix[Double] =
  //   //   Matrix(vecxt.doublearrays.*(m.raw)(-1), m.shape)

  //   def trace =
  //     if m.shape(0) != m.shape(1) then throw new IllegalArgumentException("Matrix must be square")
  //     end if
  //     m.diag.sum
  //   end trace

  //   inline def sum: Double = sumSIMD

  //   def sumSIMD: Double =
  //     if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.sum(m.raw)
  //     else
  //       var acc = 0.0
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         acc += m.raw(idx)
  //       }
  //       acc
  //     end if
  //   end sumSIMD

  //   def norm: Double =
  //     if m.hasSimpleContiguousMemoryLayout then vecxt.all.norm(m.raw)
  //     else
  //       var acc = 0.0
  //       m.layout.foreach2D { (i, j) =>
  //         val idx = m.layout.linearIndex(i, j)
  //         acc += m.raw(idx) * m.raw(idx)
  //       }
  //       Math.sqrt(acc)
  //     end if
  //   end norm

  //   // Note: det method is provided by platform-specific implementations
  //   // See: vecxt.JvmDeterminant (JVM with SIMD) and vecxt.JsNativeDeterminant (JS/Native)

  //   // inline def >=(d: Double): Matrix[Boolean] =

  //   // inline def >=(d: Double): Matrix[Boolean] =
  //   //   Matrix[Boolean](m.raw >= d, m.shape)

  //   // inline def >(d: Double): Matrix[Boolean] =
  //   //   Matrix(m.raw.gt(d), m.shape)
  // // inline def <=(d: Double): Matrix[Boolean] =
  // //   Matrix(m.raw.lte(d), m.shape)
  // // inline def <(d: Double): Matrix[Boolean] =
  // //   Matrix(m.raw.lt(d), m.shape)
  end extension
end FloatMatrix
