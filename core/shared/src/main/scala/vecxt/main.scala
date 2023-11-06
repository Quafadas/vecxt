package vecxt


@main def checkBytecode =
  val a = Array[Double](1,2,3)
  val a1 = Array[Double](1,2,3)
  // val b = Array[Boolean](true, false, true)
  // val c = Array[Boolean](false, true, true)

  import vecxt.BoundsCheck.yes
  a - a1