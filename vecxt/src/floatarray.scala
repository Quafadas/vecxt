package vecxt

object FloatArrays:
  extension (vec: Array[Float])
    inline def select(indices: Array[Int]): Array[Float] =
      val len = indices.length
      val out = Array.ofDim[Float](len)
      var i = 0
      while i < len do
        out(i) = vec(indices(i))
        i += 1
      end while
      out
    end select

    inline def unique: Array[Float] =
      if vec.size == 0 then Array.empty[Float]
      else

        val data = vec.clone()
        data.sortInPlace()
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

        val result = Array.ofDim[Float](elementCount)
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

    /** Sorts the given array in ascending order, with NaN values sorted to the end.
      *
      * This follows the IEEE total ordering implemented by `java.lang.Float.compare`, ensuring deterministic placement
      * for `NaN`, infinities, and normal values.
      */
    inline def argsort: Array[Int] =
      val n = vec.length
      if n == 0 then Array.empty[Int]
      else
        val idx = Array.tabulate(n)(identity)
        val scratch = new Array[Int](n)

        val InsertionCutoff = 32

        def insertion(lo: Int, hi: Int): Unit =
          var i = lo + 1
          while i < hi do
            val key = idx(i)
            val keyValue = vec(key)
            var j = i - 1
            while j >= lo && java.lang.Float.compare(vec(idx(j)), keyValue) > 0 do
              idx(j + 1) = idx(j)
              j -= 1
            end while
            idx(j + 1) = key
            i += 1
          end while
        end insertion

        def merge(lo: Int, mid: Int, hi: Int): Unit =
          val leftLen = mid - lo
          var i = 0
          while i < leftLen do
            scratch(i) = idx(lo + i)
            i += 1
          end while

          var left = 0
          var right = mid
          var out = lo

          while left < leftLen && right < hi do
            val leftIdx = scratch(left)
            val rightIdx = idx(right)
            if java.lang.Float.compare(vec(leftIdx), vec(rightIdx)) <= 0 then
              idx(out) = leftIdx
              left += 1
            else
              idx(out) = rightIdx
              right += 1
            end if
            out += 1
          end while

          while left < leftLen do
            idx(out) = scratch(left)
            left += 1
            out += 1
          end while
        end merge

        def sort(lo: Int, hi: Int): Unit =
          if hi - lo <= InsertionCutoff then insertion(lo, hi)
          else
            val mid = (lo + hi) >>> 1
            sort(lo, mid)
            sort(mid, hi)
            merge(lo, mid, hi)

        sort(0, n)
        idx
      end if
    end argsort
  end extension

end FloatArrays
