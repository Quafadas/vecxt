package vecxt

object DoubleArrays:
  extension (vec: Array[Double])
    // TODO bnenchmark.
    inline def select(indicies: Array[Int]): Array[Double] =
      val len = indicies.length
      val out = Array.ofDim[Double](len)
      var i = 0
      while i < len do
        out(i) = vec(indicies(i))
        i += 1
      end while
      out
    end select

    inline def unique: Array[Double] =
      if vec.size == 0 then Array.empty[Double]
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

        val result = Array.ofDim[Double](elementCount)
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

    inline private def leq(a: Double, b: Double): Boolean =
      val na = java.lang.Double.isNaN(a)
      val nb = java.lang.Double.isNaN(b)
      if na && nb then true
      else if na then false // NaN is treated as larger → goes last
      else if nb then true
      else a <= b
      end if
    end leq

    /** Sorts the given array in ascending order, with NaN values sorted to the end.
      *
      * This follows the IEEE total ordering implemented by `java.lang.Double.compare`, ensuring deterministic placement
      * for `NaN`, infinities, and normal values.
      */
    inline def argsort: Array[Int] =
      val n = vec.length
      if n == 0 then Array.empty[Int]
      else
        val idx = Array.tabulate(n)(identity)
        val scratch = new Array[Int](n)

        val InsertionCutoff = 32

        // ----- insertion sort for small slices -----
        def insertion(lo: Int, hi: Int): Unit =
          var i = lo + 1
          while i < hi do
            val key = idx(i)
            val keyValue = vec(key)
            var j = i - 1
            while j >= lo && java.lang.Double.compare(vec(idx(j)), keyValue) > 0 do
              idx(j + 1) = idx(j)
              j -= 1
            end while
            idx(j + 1) = key
            i += 1
          end while
        end insertion

        // ----- merge two sorted halves -----
        def merge(lo: Int, mid: Int, hi: Int): Unit =
          // copy left half to scratch buffer
          val leftLen = mid - lo
          var i = 0
          while i < leftLen do
            scratch(i) = idx(lo + i)
            i += 1
          end while

          var left = 0
          var right = mid
          var out = lo

          // merge based on vec values
          while left < leftLen && right < hi do
            val leftIdx = scratch(left)
            val rightIdx = idx(right)
            if java.lang.Double.compare(vec(leftIdx), vec(rightIdx)) <= 0 then
              idx(out) = leftIdx
              left += 1
            else
              idx(out) = rightIdx
              right += 1
            end if
            out += 1
          end while

          // copy remaining left side
          while left < leftLen do
            idx(out) = scratch(left)
            left += 1
            out += 1
          end while
        end merge

        // ----- recursive merge sort, with insertion cutoff -----
        def sort(lo: Int, hi: Int): Unit =
          if hi - lo <= InsertionCutoff then insertion(lo, hi)
          else
            val mid = (lo + hi) >>> 1
            sort(lo, mid)
            sort(mid, hi)
            merge(lo, mid, hi)

        // start sorting
        sort(0, n)
        idx
      end if
    end argsort
  end extension

  // inline def lt(num: Double): Array[Boolean] = vec < num

  // inline def gt(num: Double): Array[Boolean] = vec > num

  // inline def lte(num: Double): Array[Boolean] = vec <= num

  // inline def gte(num: Double): Array[Boolean] = vec >= num
  // end extension

end DoubleArrays
