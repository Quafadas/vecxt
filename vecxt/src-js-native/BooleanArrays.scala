package vecxt

object BooleanArrays:

  extension (vec: Array[Boolean])

    def allTrue = vec.forall(identity)

    def any: Boolean =
      var i = 0
      var any = false
      while i < vec.length && any == false do
        if vec(i) then any = true
        end if
        i += 1
      end while
      any
    end any

    def trues: Int =
      var i = 0
      var sum = 0
      while i < vec.length do
        if vec(i) then sum += 1
        end if
        i += 1
      end while
      sum
    end trues

    def not: Array[Boolean] =
      val result = new Array[Boolean](vec.length)
      var i = 0
      while i < vec.length do
        result(i) = !vec(i)
        i += 1
      end while
      result
    end not

    def `not!`: Unit =
      var i = 0
      while i < vec.length do
        vec(i) = !vec(i)
        i += 1
      end while
    end `not!`
  end extension
end BooleanArrays
