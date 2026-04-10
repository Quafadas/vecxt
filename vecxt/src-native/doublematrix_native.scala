package vecxt
import scala.scalanative.unsafe.*

import org.ekrich.blas.unsafe.blas
import org.ekrich.blas.unsafe.blasEnums

import vecxt.BoundsCheck.BoundsCheck
import vecxt.MatrixInstance.*
import vecxt.matrix.*

object NativeDoubleMatrix:
  extension (m: Matrix[Double])
    inline def *:*(bmat: Matrix[Boolean])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        val newArr = Array.ofDim[Double](m.rows * m.cols)
        var i = 0
        while i < newArr.length do
          newArr(i) = if bmat.raw(i) then m.raw(i) else 0.0
          i += 1
        end while
        Matrix[Double](newArr, (m.rows, m.cols))
      else
        val newArr = Array.ofDim[Double](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val mIdx = m.offset + i * m.rowStride + j * m.colStride
            val bIdx = bmat.offset + i * bmat.rowStride + j * bmat.colStride
            newArr(i + j * m.rows) = if bmat.raw(bIdx) then m.raw(mIdx) else 0.0
            j += 1
          end while
          i += 1
        end while
        Matrix[Double](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)
      end if
    end *:*

    inline def +=(arr: Array[Double])(using inline boundsCheck: BoundsCheck): Unit =

      if boundsCheck then assert(arr.length == m.cols, s"Array length ${arr.length} != expected ${m.cols}")
      end if

      var i = 0
      while i < m.rows do
        var j = 0
        while j < m.cols do
          m(i, j) = m(i, j) + arr(j)
          j += 1
        end while
        i += 1
      end while

    end +=

    inline def +=(n: Double): Unit =
      import vecxt.BoundsCheck.DoBoundsCheck.no
      if m.hasSimpleContiguousMemoryLayout then vecxt.doublearrays.+=(m.raw)(n)
      else
        // Cache-friendly fallback: iterate with smallest stride in inner loop
        if m.rowStride <= m.colStride then
          // Row stride is smaller, so iterate rows in inner loop
          var j = 0
          while j < m.cols do
            var i = 0
            while i < m.rows do
              m(i, j) = n + m(i, j)
              i += 1
            end while
            j += 1
          end while
        else
          // Column stride is smaller, so iterate columns in inner loop
          var i = 0
          while i < m.rows do
            var j = 0
            while j < m.cols do
              m(i, j) = n + m(i, j)
              j += 1
            end while
            i += 1
          end while
        end if
      end if

    end +=

    inline def >=(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Boolean](doublearrays.>=(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = m.raw(srcIdx) >= d
            j += 1
          end while
          i += 1
        end while
        Matrix[Boolean](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)

    inline def >(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Boolean](doublearrays.>(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = m.raw(srcIdx) > d
            j += 1
          end while
          i += 1
        end while
        Matrix[Boolean](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)
      end if
    end >

    inline def <=(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Boolean](doublearrays.<=(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = m.raw(srcIdx) <= d
            j += 1
          end while
          i += 1
        end while
        Matrix[Boolean](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)
      end if
    end <=

    inline def <(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Boolean](doublearrays.<(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        var i = 0
        while i < m.rows do
          var j = 0
          while j < m.cols do
            val srcIdx = m.offset + i * m.rowStride + j * m.colStride
            newArr(i + j * m.rows) = m.raw(srcIdx) < d
            j += 1
          end while
          i += 1
        end while
        Matrix[Boolean](newArr, m.rows, m.cols)(using BoundsCheck.DoBoundsCheck.no)

    inline def `matmulInPlace!`(
        b: Matrix[Double],
        c: Matrix[Double],
        alpha: Double = 1.0,
        beta: Double = 0.0
    )(using inline boundsCheck: BoundsCheck): Unit =
      dimMatCheck(m, b)

      if m.hasSimpleContiguousMemoryLayout && b.hasSimpleContiguousMemoryLayout then
        val lda = if m.isDenseColMajor then m.rows else m.cols
        val ldb = if b.isDenseColMajor then b.rows else b.cols
        val transB = if b.isDenseColMajor then blasEnums.CblasNoTrans else blasEnums.CblasTrans
        val transA = if m.isDenseColMajor then blasEnums.CblasNoTrans else blasEnums.CblasTrans

        blas.cblas_dgemm(
          if m.isDenseRowMajor && b.isDenseRowMajor then blasEnums.CblasRowMajor else blasEnums.CblasColMajor,
          transA,
          transB,
          m.rows,
          b.cols,
          m.cols,
          alpha,
          m.raw.at(0),
          lda,
          b.raw.at(0),
          ldb,
          beta,
          c.raw.at(0),
          m.rows
        )
      else if m.rowStride == 1 || m.colStride == 1 && b.rowStride == 1 || b.colStride == 1 then
        val transB = if b.rowStride == 1 then blasEnums.CblasNoTrans else blasEnums.CblasTrans
        val transA = if m.rowStride == 1 then blasEnums.CblasNoTrans else blasEnums.CblasTrans
        blas.cblas_dgemm(
          if m.isDenseRowMajor && b.isDenseRowMajor then blasEnums.CblasRowMajor else blasEnums.CblasColMajor,
          transA,
          transB,
          m.rows,
          b.cols,
          m.cols,
          alpha,
          m.raw.at(m.offset),
          if m.rowStride == 1 then m.colStride else m.rowStride,
          b.raw.at(b.offset),
          if b.rowStride == 1 then b.colStride else b.rowStride,
          beta,
          c.raw.at(c.offset),
          m.rows
        )
      else ???

      end if
    end `matmulInPlace!`

    inline def *(vec: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] =

      if m.hasSimpleContiguousMemoryLayout then
        val newArr = Array.ofDim[Double](m.rows)
        blas.cblas_dgemv(
          if m.isDenseColMajor then blasEnums.CblasColMajor else blasEnums.CblasRowMajor,
          blasEnums.CblasNoTrans,
          m.rows,
          m.cols,
          1.0,
          m.raw.at(0),
          m.rows,
          vec.at(0),
          1,
          0.0,
          newArr.at(0),
          1
        )
        newArr
      else ???
    end *
  end extension

end NativeDoubleMatrix

object JvmDoubleMatrix:

end JvmDoubleMatrix
