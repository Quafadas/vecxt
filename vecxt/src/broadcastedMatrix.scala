package vecxt
import vecxt.BoundsCheck.BoundsCheck

import narr.*
import vecxt.matrix.*
import vecxt.MatrixHelper.*
import scala.reflect.ClassTag

object Broadcasting:

  /** Experimenting with broadcasting.
    *
    * It is assumed, that the most common use cases for follow up operations, are elementwise operations.
    *
    * Therefore, the implementation can be very sparse, as it assumes one will eject back into Matrix[] as soon as
    * possible.
    */
  class BroadCastedMatrix[@specialized(Double, Int) T: ClassTag](m: Matrix[T]):

    def /(other: Matrix[T])(using f: Fractional[T], BoundsCheck): Matrix[T] =

      val raw = NArray.ofSize[T](other.raw.size)
      // var i = 0
      // while i < m.rows do
      //   var j = 0
      //   val colValue = other.raw(i)
      //   while j < m.cols do
      //     raw(i * m.cols + j) = m(i, j) / other(i, j)
      //     j += 1
      //   end while
      //   i += 1
      // end while
      Matrix(raw, other.shape)(using vecxt.BoundsCheck.DoBoundsCheck.no)
    end /

  end BroadCastedMatrix
end Broadcasting
