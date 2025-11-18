package vecxt

import narr.*

object DoubleArrays:
  extension (vec: NArray[Double])
    inline def unique: NArray[Double] =
      if vec.size == 0 then NArray.empty[Double]
      else

        val data = narr.copy[Double](vec)
        narr.sort(data)()
        var elementCount = 1
        var lastElement = data(0)

        var i = 0
        while i < data.length do
          val di = data(i)
          if di != lastElement then
            elementCount += 1
            lastElement = di
          end if
          i += 1
        end while

        val result = NArray.ofSize[Double](elementCount)
        result(0) = data(0)
        lastElement = data(0)
        var idx = 1
        i = 0
        while i < data.length do
          val di = data(i)
          if di != lastElement then
            result(idx) = di
            lastElement = di
            idx += 1
          end if
          i += 1
        end while

        result

    inline def argsort: NArray[Int] =
      if vec.size == 0 then NArray.empty[Int]
      else
        val indices = NArray.tabulate(vec.length)(i => i)

        // Insertion sort on indices based on vec values
        var i = 1
        while i < indices.length do
          val key = indices(i)
          val keyValue = vec(key)
          var j = i - 1

          while j >= 0 && vec(indices(j)) > keyValue do
            indices(j + 1) = indices(j)
            j -= 1
          end while

          indices(j + 1) = key
          i += 1
        end while

        indices
  end extension

  // inline def lt(num: Double): NArray[Boolean] = vec < num

  // inline def gt(num: Double): NArray[Boolean] = vec > num

  // inline def lte(num: Double): NArray[Boolean] = vec <= num

  // inline def gte(num: Double): NArray[Boolean] = vec >= num
  // end extension

end DoubleArrays
