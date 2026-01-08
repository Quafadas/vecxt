/** In [1]: import numpy as np ...: from scipy import linalg
  *
  * In [2]: A = np.array( ...: [ ...: [1, 9, 2, 1, 1], ...: [10, 1, 2, 1, 1], ...: [1, 0, 5, 1, 1], ...: [2, 1, 1, 2,
  * 9], ...: [2, 1, 2, 13, 2], ...: ] ...: )
  *
  * In [3]: b = np.array([170, 180, 140, 180, 350]).reshape((5, 1))
  *
  * In [4]: A_inv = linalg.inv(A)
  *
  * In [5]: x = A_inv @ b ...: x Out[5]: array([[10.], [10.], [20.], [20.], [10.]])
  */

//./mill experiments.runMain experiments.inv_test

@main def inv_test =
  import vecxt.all.*
  import vecxt.BoundsCheck.DoBoundsCheck.yes

  val A = Matrix.fromRows(
    Array(1.0, 9.0, 2.0, 1.0, 1.0),
    Array(10.0, 1.0, 2.0, 1.0, 1.0),
    Array(1.0, 0.0, 5.0, 1.0, 1.0),
    Array(2.0, 1.0, 1.0, 2.0, 9.0),
    Array(2.0, 1.0, 2.0, 13.0, 2.0)
  )

  val b = Array(170.0, 180.0, 140.0, 180.0, 350.0)

  val A_inv = inv(A)

  val x = A_inv * b

  println(x.argsort.printArr)
  println(x.printArr)

  println(A.det)

  println((pinv(A) * b).printArr)
end inv_test

@main def argmax =
  import vecxt.all.*
  import vecxt.BoundsCheck.DoBoundsCheck.yes

  val r1 = Matrix.fromRows(
    Array(0.0, 1.0, 2.0, 3.0),
    Array(4.0, 5.0, 6.0, 7.0),
    Array(8.0, 9.0, 10.0, 11.0),
    Array(12.0, 13.0, 14.0, 15.0),
    Array(16.0, 17.0, 18.0, 19.0)
  )

  println(r1.raw.argmax)
  println(r1.mapRowsToScalar(r => r.argmax).printMat)
  println(r1.mapColsToScalar(c => c.argmax).printMat)
end argmax
