package vecxt_re

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

/** Compute the sum of values for each group identified by an integer index.
  *
  * The function expects `groups` to be sorted in non-decreasing order and that `groups` and `values` have the same
  * length. Group indices are 1-based and must be in the range 1..nitr. The returned array has length `nitr`; element at
  * position `i` (0-based) contains the sum of values for group index `i+1`. Groups with no entries produce a zero in
  * the corresponding slot.
  *
  * Preconditions:
  *   - groups.length == values.length
  *   - groups is sorted (runs of identical indices are contiguous)
  *   - every g in groups satisfies 1 <= g <= nitr
  *
  * Complexity: O(groups.length) time, O(nitr) extra space.
  *
  * This method is unsafe and performs no checks that these conditions are satisfied. It is the responsibility of the
  * caller.
  *
  * @param groups
  *   sorted array of 1-based group indices (length L)
  * @param values
  *   array of values corresponding to each group index (length L)
  * @param nitr
  *   number of groups (size of the returned array)
  * @return
  *   an Array[Double] of length `nitr` where each element is the sum for that group
  * @throws ArrayIndexOutOfBoundsException
  *   if a group index is outside 1..nitr
  * @throws IllegalArgumentException
  *   if groups.length != values.length
  *
  * Example: groups = Array(1, 1, 3), values = Array(1.0, 2.0, 4.0), nitr = 4 result = Array(3.0, 0.0, 4.0, 0.0)
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

/**   - count by group index
  *   - Each group has a small number of values.
  *   - Each the groups are keyed by their index.
  *   - assumes groups are already sorted
  */
inline def groupCount(groups: Array[Int], nitr: Int): Array[Int] =
  val result = Array.fill(nitr)(0)
  val l = groups.length
  var i = 0
  while i < l do
    val g = groups(i)
    var groupSum = 0
    // Process block of same group, computing cumulative sum
    while i < l && groups(i) == g do
      groupSum += 1
      i += 1
    end while
    result(g - 1) = groupSum
  end while

  result
end groupCount

/** Compute the maximum of values for each group identified by an integer index.
  *
  * The function expects `groups` to be sorted in non-decreasing order and that `groups` and `values` have the same
  * length. Group indices are 1-based and must be in the range 1..nitr. The returned array has length `nitr`; element at
  * position `i` (0-based) contains the max of values for group index `i+1`. Groups with no entries produce
  * Double.NegativeInfinity in the corresponding slot.
  *
  * Preconditions:
  *   - groups.length == values.length
  *   - groups is sorted (runs of identical indices are contiguous)
  *   - every g in groups satisfies 1 <= g <= nitr
  *
  * Complexity: O(groups.length) time, O(nitr) extra space.
  *
  * This method is unsafe and performs no checks that these conditions are satisfied. It is the responsibility of the
  * caller.
  *
  * @param groups
  *   sorted array of 1-based group indices (length L)
  * @param values
  *   array of values corresponding to each group index (length L)
  * @param nitr
  *   number of groups (size of the returned array)
  * @return
  *   an Array[Double] of length `nitr` where each element is the max for that group
  * @throws ArrayIndexOutOfBoundsException
  *   if a group index is outside 1..nitr
  * @throws IllegalArgumentException
  *   if groups.length != values.length
  *
  * Example: groups = Array(1, 1, 3), values = Array(1.0, 2.0, 4.0), nitr = 4 result = Array(2.0, -Inf, 4.0, -Inf)
  */
inline def groupMax(groups: Array[Int], values: Array[Double], nitr: Int): Array[Double] =
  val result = Array.fill(nitr)(Double.NegativeInfinity)
  val l = groups.length
  var i = 0
  while i < l do
    val g = groups(i)
    var groupMax = Double.NegativeInfinity
    // Process block of same group, computing max
    while i < l && groups(i) == g do
      if values(i) > groupMax then groupMax = values(i)
      i += 1
    end while
    result(g - 1) = groupMax
  end while

  result
end groupMax

