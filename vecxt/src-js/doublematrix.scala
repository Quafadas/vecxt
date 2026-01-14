package vecxt

import scala.scalajs.js.typedarray.Float64Array

import vecxt.BoundsCheck.BoundsCheck
import vecxt.matrix.*
import scala.scalajs.js.JSConverters.*

object JsDoubleMatrix:

  extension (m: Matrix[Double])

    inline def `matmulInPlace!`(b: Matrix[Double], c: Matrix[Double], alpha: Double = 1.0, beta: Double = 0.0)(using
        inline boundsCheck: BoundsCheck
    ): Unit =
      dimMatCheck(m, b)
      inline if boundsCheck then
        println("PERFORMING WARNING in matmul on JS")
        println("THIS method copies into native JS types. Then copies back out. Expect catastrophic performance.")
      end if

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

    inline def *(vec: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] =
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
