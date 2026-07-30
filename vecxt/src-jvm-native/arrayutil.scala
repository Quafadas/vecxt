package vecxt

object arrayUtil:

  extension [A](d: Array[A]) def printArr: String = d.mkString("[", ",", "]")
  end extension

  // Concrete overloads (vecxt/issues/105, check C6a): a concretely-typed receiver resolves `.mkString`
  // through Predef's type-specific wrap (wrapDoubleArray etc.) instead of the generic fallback.
  extension (d: Array[Double]) def printArr: String = d.mkString("[", ",", "]")
  end extension

  extension (d: Array[Float]) def printArr: String = d.mkString("[", ",", "]")
  end extension

  extension (d: Array[Int]) def printArr: String = d.mkString("[", ",", "]")
  end extension

  extension (d: Array[Long]) def printArr: String = d.mkString("[", ",", "]")
  end extension

  extension (d: Array[Boolean]) def printArr: String = d.mkString("[", ",", "]")
  end extension

end arrayUtil
