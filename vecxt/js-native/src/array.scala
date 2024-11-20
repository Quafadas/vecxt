package vecxt

import narr.*
import vecxt.matrix.Matrix

// These use project panama (SIMD) on the JVM, so need own JS native implementation
object JsNativeDoubleArrays:

  extension (m: Matrix[Double])
    inline def >=(d: Double): Matrix[Boolean] =
      Matrix[Boolean](m.raw >= d, m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def >(d: Double): Matrix[Boolean] =
      Matrix[Boolean](m.raw > d, m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def <=(d: Double): Matrix[Boolean] =
      Matrix[Boolean](m.raw <= d, m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def <(d: Double): Matrix[Boolean] =
      Matrix[Boolean](m.raw < d, m.shape)(using BoundsCheck.DoBoundsCheck.no)
  end extension

  extension (vec: NArray[Double])

    inline def <(num: Double): NArray[Boolean] =
      logicalIdx((a, b) => a < b, num)

    inline def <=(num: Double): NArray[Boolean] =
      logicalIdx((a, b) => a <= b, num)

    inline def >(num: Double): NArray[Boolean] =
      logicalIdx((a, b) => a > b, num)

    inline def >=(num: Double): NArray[Boolean] =
      logicalIdx((a, b) => a >= b, num)

    inline def logicalIdx(
        inline op: (Double, Double) => Boolean,
        inline num: Double
    ): NArray[Boolean] =
      val n = vec.length
      val idx = NArray.fill(n)(false)

      var i = 0
      while i < n do
        if op(vec(i), num) then idx(i) = true
        end if
        i = i + 1
      end while
      idx
    end logicalIdx
  end extension
end JsNativeDoubleArrays
