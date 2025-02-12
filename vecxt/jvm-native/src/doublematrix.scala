package vecxt
import vecxt.BoundsCheck.BoundsCheck
import vecxt.arrays.*
import vecxt.matrix.*

object JvmNativeDoubleMatrix:
  extension (m: Matrix[Double])

    inline def +(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, m2)
      val newArr = m.raw.add(m2.raw)
      Matrix(newArr, m.shape)(using BoundsCheck.DoBoundsCheck.no)
    end +
  end extension
end JvmNativeDoubleMatrix

object JsDoubleMatrix:

end JsDoubleMatrix
