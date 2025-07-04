package vecxt

import scala.util.control.Breaks.*
import narr.*

object IntArrays:
  extension (arr: NArray[Int])
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
  end extension

end IntArrays
