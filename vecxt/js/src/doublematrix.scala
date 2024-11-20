package vecxt

import narr.*
import matrix.*
import vecxt.MatrixHelper.*
import vecxt.MatrixInstance.*
import scala.scalajs.js.typedarray.Float64Array
import vecxt.BoundsCheck.BoundsCheck
import vecxt.arrays.*
import vecxt.rangeExtender.MatrixRange.range
import vecxt.rangeExtender.MatrixRange.RangeExtender

object JsDoubleMatrix:

  extension (m: Matrix[Double])

    // inline def tupleFromIdx(b: Int)(using inline boundsCheck: BoundsCheck) =
    //   dimCheckLen(m.raw, b)
    //   (b / m.rows, b % m.rows)
    // end tupleFromIdx

    inline def +(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, m2)
      val newArr: NArray[Double] = m.raw.add(m2.raw)
      Matrix[Double](newArr, m.shape)(using BoundsCheck.DoBoundsCheck.no)
    end +

    inline def matmul(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      dimMatCheck(m, b)
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
    end matmul
  end extension
end JsDoubleMatrix

object JvmDoubleMatrix:

end JvmDoubleMatrix

object NativeDoubleMatrix:

end NativeDoubleMatrix

object JvmNativeDoubleMatrix:

end JvmNativeDoubleMatrix
