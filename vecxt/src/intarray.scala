package vecxt

import scala.reflect.ClassTag
import scala.util.control.Breaks.*

import vecxt.BooleanArrays.trues
import vecxt.BoundsCheck.BoundsCheck
import vecxt.arrays.sumSIMD

object IntArrays:

  extension [A](vec: Array[A])
    inline def mask(index: Array[Boolean])(using inline boundsCheck: BoundsCheck, ct: ClassTag[A]) =
      dimCheck(vec, index)
      val trues = index.trues
      val newVec: Array[A] = new Array[A](trues)
      var j = 0
      for i <- 0 until index.length do
        // println(s"i: $i  || j: $j || ${index(i)} ${vec(i)} ")
        if index(i) then
          newVec(j) = vec(i)
          j = 1 + j
      end for
      newVec
    end mask
  end extension
  extension (arr: Array[Int])
    inline def select(indicies: Array[Int]): Array[Int] =
      val len = indicies.length
      val out = Array.ofDim[Int](len)
      var i = 0
      while i < len do
        out(i) = arr(indicies(i))
        i += 1
      end while
      out
    end select

    inline def contiguous: Boolean =
      var i = 1
      var out = true
      breakable {
        while i < arr.length do
          if arr(i) != arr(i - 1) + 1 then
            out = false
            break
          end if
          i += 1
        end while
      }
      out
    end contiguous
  end extension

end IntArrays
