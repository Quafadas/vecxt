package vecxt

object intarrays:

  extension (vec: Array[Int])

    def increments: Array[Int] =
      val n = vec.length
      val idx = Array.ofDim[Int](vec.length)

      var i = 1
      while i < n do
        idx(i) = vec(i) - vec(i - 1)
        i = i + 1
      end while
      idx(0) = vec(0)
      idx
    end increments

    def -(other: Array[Int]): Array[Int] =
      dimCheck(vec, other)
      val n = vec.length
      val res = Array.fill(n)(0)

      var i = 0
      while i < n do
        res(i) = vec(i) - other(i)
        i = i + 1
      end while
      res
    end -

    def +(other: Array[Int]): Array[Int] =
      dimCheck(vec, other)

      val n = vec.length
      val res = Array.fill(n)(0)

      var i = 0
      while i < n do
        res(i) = vec(i) + other(i)
        i = i + 1
      end while
      res
    end +

    def dot(other: Array[Int]): Int =
      dimCheck(vec, other)
      val n = vec.length
      var sum = 0

      var i = 0
      while i < n do
        sum += vec(i) * other(i)
        i = i + 1
      end while
      sum
    end dot

    def =:=(nums: Array[Int]): Array[Boolean] =
      logicalIdxArr(nums, (a, b) => a == b)

    def =:=(num: Int): Array[Boolean] =
      logicalIdx((a, b) => a == b, num)

    def <(num: Int): Array[Boolean] =
      logicalIdx((a, b) => a < b, num)

    def <=(num: Int): Array[Boolean] =
      logicalIdx((a, b) => a <= b, num)

    def >(num: Int): Array[Boolean] =
      logicalIdx((a, b) => a > b, num)

    def >=(num: Int): Array[Boolean] =
      logicalIdx((a, b) => a >= b, num)

    inline def logicalIdx(
        inline op: (Int, Int) => Boolean,
        inline num: Int
    ): Array[Boolean] =
      val n = vec.length
      val idx = Array.fill(n)(false)

      var i = 0
      while i < n do
        if op(vec(i), num) then idx(i) = true
        end if
        i = i + 1
      end while
      idx
    end logicalIdx

    inline def logicalIdxArr(
        compare: Array[Int],
        inline op: (Int, Int) => Boolean
    ): Array[Boolean] =
      val n = vec.length
      val idx = Array.fill(n)(false)

      var i = 0
      while i < n do
        if op(vec(i), compare(i)) then idx(i) = true
        end if
        i = i + 1
      end while
      idx
    end logicalIdxArr

    def countsToIdx: Array[Int] =
      var total = vec.sum
      var i = 0
      val out = new Array[Int](total)
      var j = 0
      while i < vec.length do
        val count = vec(i)
        val idx = i + 1
        var k = 0
        while k < count do
          out(j) = idx
          j += 1
          k += 1
        end while
        i += 1
      end while
      out
    end countsToIdx

    def mean: Double =
      var sum = 0.0
      var i = 0
      while i < vec.length do
        sum += vec(i)
        i += 1
      end while
      sum / vec.length
    end mean

    def variance: Double = variance(VarianceMode.Population)

    def variance(mode: VarianceMode): Double =
      vec.meanAndVariance(mode).variance
    end variance

    def meanAndVariance: MeanAndVariance =
      meanAndVariance(VarianceMode.Population)

    def meanAndVariance(mode: VarianceMode): MeanAndVariance =
      var mean = 0.0
      var m2 = 0.0
      var i = 0
      while i < vec.length do
        val n = i + 1
        val delta = vec(i) - mean
        mean += delta / n
        val delta2 = vec(i) - mean
        m2 += delta * delta2
        i += 1
      end while
      val denom = mode match
        case VarianceMode.Population => vec.length.toDouble
        case VarianceMode.Sample     => (vec.length - 1).toDouble

      MeanAndVariance(mean, m2 / denom)
    end meanAndVariance

    def std: Double = std(VarianceMode.Population)

    def std(mode: VarianceMode): Double =
      Math.sqrt(vec.variance(mode))

    def stdDev: Double = stdDev(VarianceMode.Population)

    def stdDev(mode: VarianceMode): Double = std(mode)

    def minSIMD: Int =
      var i = 0
      var acc = Int.MaxValue
      while i < vec.length do
        val v = vec(i)
        if v < acc then acc = v
        end if
        i += 1
      end while
      acc
    end minSIMD

    def maxSIMD: Int =
      var i = 0
      var acc = Int.MinValue
      while i < vec.length do
        val v = vec(i)
        if v > acc then acc = v
        end if
        i += 1
      end while
      acc
    end maxSIMD

    def -=(scalar: Int): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) - scalar
        i += 1
      end while
    end -=

    def /(scalar: Double): Array[Double] =
      val result = new Array[Double](vec.length)
      var i = 0
      while i < vec.length do
        result(i) = vec(i) / scalar
        i += 1
      end while
      result
    end /

    def /(scalar: Float): Array[Float] =
      val result = new Array[Float](vec.length)
      var i = 0
      while i < vec.length do
        result(i) = vec(i) / scalar
        i += 1
      end while
      result
    end /

    def *(scalar: Float): Array[Float] =
      val result = new Array[Float](vec.length)
      var i = 0
      while i < vec.length do
        result(i) = vec(i) * scalar
        i += 1
      end while
      result
    end *

    def -(scalar: Int): Array[Int] =
      val out = vec.clone()
      out -= scalar
      out
    end -

    def -=(vec2: Array[Int]): Unit =
      dimCheck(vec, vec2)
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) - vec2(i)
        i += 1
      end while
    end -=

    def +=(vec2: Array[Int]): Unit =
      dimCheck(vec, vec2)
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + vec2(i)
        i += 1
      end while
    end +=
  end extension

end intarrays
