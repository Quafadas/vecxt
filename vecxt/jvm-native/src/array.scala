package vecxt

import narr.*

object arrayUtil:

  extension [A](d: NArray[A]) def printArr: String = d.mkString("[", ",", "]")
end arrayUtil
