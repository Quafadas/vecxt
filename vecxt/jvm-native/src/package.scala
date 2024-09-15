package vecxt

import vecxt.MatrixStuff.Matrix
import scala.reflect.ClassTag

extension [A](m: Matrix[A])
  inline def print(using ClassTag[A]): String =
    val arrArr = for i <- 0 until m.rows yield m.row(i).mkString(" ")
    arrArr.mkString("\n")
  end print
end extension
