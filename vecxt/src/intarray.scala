package vecxt

import scala.util.control.Breaks.*

object IntArrays:
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
