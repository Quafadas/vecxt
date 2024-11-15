package vecxt

object arrayUtil:

  extension [A](d: Array[A]) def printArr: String = d.mkString("[", ",", "]")
end arrayUtil
