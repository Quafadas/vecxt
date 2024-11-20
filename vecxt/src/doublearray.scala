package vecxt

import narr.*

import vecxt.JsNativeDoubleArrays.*

import arrays.*

object DoubleArrays:
  extension (vec: NArray[Double])

    inline def lt(num: Double): NArray[Boolean] = vec < num

    inline def gt(num: Double): NArray[Boolean] = vec > num

    inline def lte(num: Double): NArray[Boolean] = vec <= num

    inline def gte(num: Double): NArray[Boolean] = vec >= num
  end extension

end DoubleArrays
