package vecxtensions

/**   - You have a sorted groups array.
  *     - Each group has a small number of values.
  *   - You're doing per-group cumulative sums.
  *   - Returns cumulative sums for each element within its group
  */
inline def groupCumSum(groups: Array[Int], values: Array[Double]): Array[Double] =

  val n = groups.length
  if n == 0 then Array.empty[Double]
  else
    val result = new Array[Double](n)

    var i = 0
    while i < n do
      val g = groups(i)
      var cumSum = 0.0

      // Process block of same group, computing cumulative sum
      while i < n && groups(i) == g do
        cumSum += values(i)
        result(i) = cumSum
        i += 1
      end while
    end while

    result
  end if
end groupCumSum

/** In-place version that modifies the input array directly. More efficient when the original values are not needed.
  */
inline def groupCumSumInPlace(groups: Array[Int], values: Array[Double]): Unit =
  val n = groups.length
  if n > 0 then
    var i = 0
    while i < n do
      val g = groups(i)
      var cumSum = 0.0

      // Process block of same group, computing cumulative sum in-place
      while i < n && groups(i) == g do
        cumSum += values(i)
        values(i) = cumSum
        i += 1
      end while
    end while
  end if
end groupCumSumInPlace
