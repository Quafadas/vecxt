package vecxt

import vecxt.BoundsCheck.BoundsCheck
import vecxt.intarrays.*
import vecxt.matrix.*
import vecxt.MatrixInstance.*
import jdk.incubator.vector.IntVector
import jdk.incubator.vector.VectorMask

object JvmIntMatrix:
  extension (m: Matrix[Int])

    inline def matmul(b: Matrix[Int])(using inline boundsCheck: BoundsCheck): Matrix[Int] =
      dimMatCheck(m, b)
      ???

    end matmul

    @scala.annotation.targetName("intMatrixDivDouble")
    inline def /(d: Double): Matrix[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Double](vecxt.intarrays./(i)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end /

    @scala.annotation.targetName("intMatrixDivFloat")
    inline def /(d: Float): Matrix[Float] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Float](vecxt.intarrays./(i)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end /

    inline def >=(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.gte(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end >=

    inline def >(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.gt(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end >

    inline def <=(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.lte(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end <=

    inline def <(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.lt(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end <

    @scala.annotation.targetName("intMatrixMaskInPlace")
    inline def *:*=(bmat: Matrix[Boolean])(using inline boundsCheck: BoundsCheck): Unit =
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
          i += 1
        end while
      else ???
      end if
    end *:*=

    @scala.annotation.targetName("intMatrixMask")
    inline def *:*(bmat: Matrix[Boolean])(using inline boundsCheck: BoundsCheck): Matrix[Int] =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        val copy = m.deepCopy
        copy *:*= bmat
        copy
      else
        val newArr = Array.ofDim[Int](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val mIdx = m.offset + i * m.rowStride + j * m.colStride
            val bIdx = bmat.offset + i * bmat.rowStride + j * bmat.colStride
            newArr(i + j * m.rows) = if bmat.raw(bIdx) then m.raw(mIdx) else 0
            j += 1
          end while
          i += 1
        end while
        Matrix[Int](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)
      end if
    end *:*

  end extension
end JvmIntMatrix

object NativeIntMatrix:

end NativeIntMatrix
