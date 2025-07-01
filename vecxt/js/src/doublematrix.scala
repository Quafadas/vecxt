package vecxt

import scala.scalajs.js.typedarray.Float64Array

import vecxt.BoundsCheck.BoundsCheck
import vecxt.matrix.*

import scala.annotation.nowarn
import narr.*

object JsDoubleMatrix:

  extension (m: Matrix[Double])

    inline def `matmulInPlace!`(b: Matrix[Double], c: Matrix[Double], alpha: Double = 1.0, beta: Double = 0.0)(using
        inline boundsCheck: BoundsCheck
    ): Unit =
      dimMatCheck(m, b)
      if m.hasSimpleContiguousMemoryLayout && b.hasSimpleContiguousMemoryLayout then
        val lda = if m.isDenseColMajor then m.rows else m.cols
        val ldb = if b.isDenseColMajor then b.rows else b.cols

        val transB = if b.isDenseColMajor then "no-transpose" else "transpose"
        val transA = if m.isDenseColMajor then "no-transpose" else "transpose"

        // Note, might need to deal with transpose later.
        dgemm(
          if m.isDenseRowMajor && b.isDenseRowMajor then "row-major" else "column-major",
          transA,
          transB,
          m.rows,
          b.cols,
          m.cols,
          alpha,
          m.raw,
          lda,
          b.raw,
          ldb,
          beta,
          c.raw,
          m.rows
        )
      else ???
      end if

    end `matmulInPlace!`

    inline def *(vec: NArray[Double])(using inline boundsCheck: BoundsCheck): NArray[Double] =
      if m.hasSimpleContiguousMemoryLayout then
        val newArr = Float64Array(m.rows)
        dgemv(
          if m.isDenseColMajor then "column-major" else "row-major",
          "no-transpose",
          m.rows,
          m.cols,
          1.0,
          m.raw,
          m.rows,
          vec,
          1,
          0.0,
          newArr,
          1
        )
        newArr
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
