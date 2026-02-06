package vecxt

object LongArrays:

  extension (arr: Array[Long])
    inline def select(indicies: Array[Int]): Array[Long] =
      val len = indicies.length
      val out = Array.ofDim[Long](len)
      var i = 0
      while i < len do
        out(i) = arr(indicies(i))
        i += 1
      end while
      out
    end select

    inline def sumSIMD: Long = ???
  end extension

end LongArrays
