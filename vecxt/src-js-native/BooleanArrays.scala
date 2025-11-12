package vecxt
import narr.*

object BooleanArrays:

  extension (vec: NArray[Boolean])

    inline def allTrue = vec.forall(identity)

    inline def any: Boolean =
      var i = 0
      var any = false
      while i < vec.length && any == false do
        if vec(i) then any = true
        end if
        i += 1
      end while
      any
    end any

    inline def trues: Int =
      var i = 0
      var sum = 0
      while i < vec.length do
        if vec(i) then sum += 1
        end if
        i += 1
      end while
      sum
    end trues
  end extension
end BooleanArrays
