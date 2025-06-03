package vecxt

import narr.*
import vecxt.JsNativeDoubleArrays.*
import arrays.*

object DoubleArrays:
  extension (vec: NArray[Double])
    inline def unique: NArray[Double] = 
      if (vec.size == 0) {
        NArray.empty[Double]
      } else {        
        
        val data = narr.copy[Double](vec)
        narr.sort(data)()
        var elementCount = 1
        var lastElement = data(0)

        var i = 0
        while (i < data.length) {
          val di = data(i)
          if (di != lastElement) {
            elementCount += 1
            lastElement = di
          }
          i += 1
        }

        val result = NArray.ofSize[Double](elementCount)
        result(0) = data(0)
        lastElement = data(0)
        var idx = 1
        i = 0
        while (i < data.length) {
          val di = data(i)
          if (di != lastElement) {
            result(idx) = di
            lastElement = di
            idx += 1
          }
          i += 1
        }

        result
      }

  // inline def lt(num: Double): NArray[Boolean] = vec < num

  // inline def gt(num: Double): NArray[Boolean] = vec > num

  // inline def lte(num: Double): NArray[Boolean] = vec <= num

  // inline def gte(num: Double): NArray[Boolean] = vec >= num
  // end extension

end DoubleArrays
