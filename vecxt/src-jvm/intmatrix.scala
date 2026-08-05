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

    def matmul(b: Matrix[Int]): Matrix[Int] =
      dimMatCheck(m, b)
      ???

    end matmul

    @scala.annotation.targetName("intMatrixDivDouble")
    def /(d: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Double](vecxt.intarrays./(i)(d), m.layout)
      else ???
    end /

    @scala.annotation.targetName("intMatrixDivFloat")
    def /(d: Float): Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Float](vecxt.intarrays./(i)(d), m.layout)
      else ???
    end /

    def >=(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.gte(d), m.layout)
      else ???
    end >=

    def >(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.gt(d), m.layout)
      else ???
    end >

    def <=(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.lte(d), m.layout)
      else ???
    end <=

    def <(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.lt(d), m.layout)
      else ???
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
      else ???
      end if
    end *:*=

    @scala.annotation.targetName("intMatrixMask")
    def *:*(bmat: Matrix[Boolean]): Matrix[Int] =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        // Preserve m's own orientation: deepCopy defaults to column-major, which would mismatch bmat's
        // orientation whenever m (and hence bmat, per the check above) is dense row-major, sending the
        // `*:*=` below into its `else ???` fallback instead of the fast path both were just confirmed to share.
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
