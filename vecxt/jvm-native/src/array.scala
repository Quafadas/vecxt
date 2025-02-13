package vecxt

import narr.*

object arrayUtil:

  extension [A](d: NArray[A]) def printArr: String = d.mkString("[", ",", "]")
  end extension

  extension (vec: NArray[Double])
    inline def exp: Array[Double] = vec.clone.map(Math.exp)

    inline def log: Array[Double] = vec.clone.map(Math.log)
  end extension

  // end extension
end arrayUtil
