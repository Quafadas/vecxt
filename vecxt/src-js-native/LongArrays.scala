package vecxt

object LongArrays:

  extension (arr: Array[Long])
    def select(indicies: Array[Int]): Array[Long] =
      val len = indicies.length
      val out = Array.ofDim[Long](len)
      var i = 0
      while i < len do
        out(i) = arr(indicies(i))
        i += 1
      end while
      out
    end select

    /** No SIMD vector API is available on JS/Native, so this is a plain scalar sum. Kept as `sumSIMD` to match the
      * name used by the JVM implementation (backed by `jdk.incubator.vector`).
      */
    def sumSIMD: Long =
      val len = arr.length
      var total = 0L
      var i = 0
      while i < len do
        total += arr(i)
        i += 1
      end while
      total
    end sumSIMD
  end extension

end LongArrays
