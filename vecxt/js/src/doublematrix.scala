package vecxt

import scala.scalajs.js.typedarray.Float64Array

import vecxt.BoundsCheck.BoundsCheck
import vecxt.matrix.*

import scala.annotation.nowarn
import narr.*

object JsDoubleMatrix:

  extension (m: Matrix[Double])

    inline def matmul(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      dimMatCheck(m, b)
      if m.hasSimpleContiguousMemoryLayout && b.hasSimpleContiguousMemoryLayout then
        val newArr = Float64Array(m.rows * b.cols)
        // Note, might need to deal with transpose later.
        dgemm(
          "column-major",
          "no-transpose",
          "no-transpose",
          m.rows,
          b.cols,
          m.cols,
          1.0,
          m.raw,
          m.rows,
          b.raw,
          b.rows,
          1.0,
          newArr,
          m.rows
        )
        Matrix[Double](newArr, (m.rows, b.cols))
      else
        ???

    end matmul
  end extension
end JsDoubleMatrix

object JvmDoubleMatrix:

end JvmDoubleMatrix

object NativeDoubleMatrix:

end NativeDoubleMatrix

object JvmNativeDoubleMatrix:

end JvmNativeDoubleMatrix
