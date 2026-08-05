package vecxt

import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.Float64Array

import vecxt.MatrixInstance.*
import vecxt.matrix.*

object JsDoubleMatrix:

  extension (m: Matrix[Double])

    def >=(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](doublearrays.>=(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) >= d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    def >(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](doublearrays.>(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) > d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)
      end if
    end >

    def <=(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](doublearrays.<=(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) <= d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)
      end if
    end <=

    def <(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](doublearrays.<(m.raw)(d), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          val srcIdx = m.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = m.raw(srcIdx) < d
        }
        Matrix[Boolean](newArr, m.rows, m.cols)

    def *:*(bmat: Matrix[Boolean]): Matrix[Double] =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        val newArr = Array.ofDim[Double](m.rows * m.cols)
        var i = 0
        while i < newArr.length do
          newArr(i) = if bmat.raw(i) then m.raw(i) else 0.0
          i += 1
        end while
        Matrix[Double](newArr, m.layout)
      else
        val newArr = Array.ofDim[Double](m.numel)
        m.layout.foreach2D { (i, j) =>
          val mIdx = m.layout.linearIndex(i, j)
          val bIdx = bmat.layout.linearIndex(i, j)
          newArr(i + j * m.rows) = if bmat.raw(bIdx) then m.raw(mIdx) else 0.0
        }
        Matrix[Double](newArr, m.rows, m.cols)
      end if
    end *:*

    def +=(arr: Array[Double]): Unit =

      assert(arr.length == m.cols, s"Array length ${arr.length} != expected ${m.cols}")

      m.layout.foreach2D { (i, j) =>
        m(i, j) = m(i, j) + arr(j)
      }

    end +=

    def +=(n: Double): Unit =
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

    def `matmulInPlace!`(b: Matrix[Double], c: Matrix[Double], alpha: Double = 1.0, beta: Double = 0.0): Unit =
      dimMatCheck(m, b)
      println("PERFORMING WARNING in matmul on JS")
      println("THIS method copies into native JS types. Then copies back out. Expect catastrophic performance.")

      if m.hasSimpleContiguousMemoryLayout && b.hasSimpleContiguousMemoryLayout then
        val lda = if m.isDenseColMajor then m.rows else m.cols
        val ldb = if b.isDenseColMajor then b.rows else b.cols

        val transB = if b.isDenseColMajor then "no-transpose" else "transpose"
        val transA = if m.isDenseColMajor then "no-transpose" else "transpose"

        // Note, might need to deal with transpose later.
        val outArr = new Float64Array(c.raw.toJSArray)
        dgemm(
          if m.isDenseRowMajor && b.isDenseRowMajor then "row-major" else "column-major",
          transA,
          transB,
          m.rows,
          b.cols,
          m.cols,
          alpha,
          new Float64Array(m.raw.toJSArray),
          lda,
          new Float64Array(b.raw.toJSArray),
          ldb,
          beta,
          outArr,
          m.rows
        )
        // copy result back into c.raw (Scala Array[Double]) element-wise
        val copyLen = Math.min(outArr.length, c.raw.length)
        var ci = 0
        while ci < copyLen do
          c.raw(ci) = outArr(ci)
          ci += 1
        end while
      else if m.rowStride == 1 || m.colStride == 1 && b.rowStride == 1 || b.colStride == 1 then
        val transB = if b.rowStride == 1 then "no-transpose" else "transpose"
        val transA = if m.rowStride == 1 then "no-transpose" else "transpose"

        val outArr = new Float64Array(c.raw.toJSArray)
        dgemm(
          if m.isDenseRowMajor && b.isDenseRowMajor then "row-major" else "column-major",
          transA,
          transB,
          m.rows,
          b.cols,
          m.cols,
          alpha,
          // convert backing Scala Array[Double] to Float64Array slice (copies)
          new Float64Array(m.raw.toJSArray).subarray(m.offset),
          if m.rowStride == 1 then m.colStride else m.rowStride,
          new Float64Array(b.raw.toJSArray).subarray(b.offset),
          if b.rowStride == 1 then b.colStride else b.rowStride,
          beta,
          outArr,
          m.rows
        )
        // copy result back into c.raw (Scala Array[Double]) element-wise
        val copyLen2 = Math.min(outArr.length, c.raw.length)
        var cj = 0
        while cj < copyLen2 do
          c.raw(cj) = outArr(cj)
          cj += 1
        end while
      else ???
      end if

    end `matmulInPlace!`

    def *(vec: Array[Double]): Array[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        val newArr = new Float64Array(m.rows)
        dgemv(
          if m.isDenseColMajor then "column-major" else "row-major",
          "no-transpose",
          m.rows,
          m.cols,
          1.0,
          new Float64Array(m.raw.toJSArray),
          m.rows,
          new Float64Array(vec.toJSArray),
          1,
          0.0,
          newArr,
          1
        )
        newArr.toArray
      else ???
    end *

  end extension

end JsDoubleMatrix

object JvmDoubleMatrix:

end JvmDoubleMatrix

object NativeDoubleMatrix:

end NativeDoubleMatrix

object JvmNativeDoubleMatrix:

end JvmNativeDoubleMatrix
