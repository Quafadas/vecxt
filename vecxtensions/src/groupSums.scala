package vecxtensions

import vecxt.reinsurance.Layer

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

/**   - sum by group index
  *   - Each group has a small number of values.
  *   - Each the groups are keyed by their index.
  *   - assumes groups are already sorted
  */
inline def groupSum(groups: Array[Int], values: Array[Double], nitr: Int): Array[Double] =
  val result = Array.fill(nitr)(0.0)
  val l = groups.length
  var i = 0
  while i < l do
    val g = groups(i)
    var groupSum = 0.0
    // Process block of same group, computing cumulative sum
    while i < l && groups(i) == g do
      groupSum += values(i)
      i += 1
    end while
    result(g - 1) = groupSum
  end while

  result
end groupSum