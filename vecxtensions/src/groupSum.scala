package vecxtensions

/**   - You have a sorted groups array.
  *     - Each group has a small number of values.
  *   - You’re doing per-group reductions.
  *   - Does not fill in any "gaps" in the groups array
  */
def groupSum(groups: Array[Int], values: Array[Double]): (uniqueGroups: Array[Int], groupSums: Array[Double]) =

  val n = groups.length
  if n == 0 then return (Array.empty[Int], Array.empty[Double])
  end if

  // Single pass: collect groups and sums using growable arrays
  val uniqueGroupsBuilder = Array.newBuilder[Int]
  val groupSumsBuilder = Array.newBuilder[Double]

  var i = 0
  while i < n do
    val g = groups(i)
    var sum = 0.0
    uniqueGroupsBuilder += g

    // Process block of same group
    while i < n && groups(i) == g do
      sum += values(i)
      i += 1
    end while

    groupSumsBuilder += sum
  end while

  (uniqueGroups = uniqueGroupsBuilder.result(), groupSums = groupSumsBuilder.result())
end groupSum

/**   - You have a sorted groups array.
  *     - Each group has a small number of values.
  *   - You're doing per-group cumulative sums.
  *   - Returns cumulative sums for each element within its group
  */
def groupCumSum(groups: Array[Int], values: Array[Double]): Array[Double] =

  val n = groups.length
  if n == 0 then return Array.empty[Double]
  end if

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
end groupCumSum

/**   - You have a sorted groups array.
  *     - Each group has a small number of values.
  *   - You're doing per-group differences.
  *   - Returns differences between consecutive elements within each group
  *   - Each group starts with its first value
  */
def groupDiff(groups: Array[Int], values: Array[Double]): Array[Double] =

  val n = groups.length
  if n == 0 then return Array.empty[Double]
  end if

  val result = new Array[Double](n)

  var i = 0
  while i < n do
    val g = groups(i)
    var prevValue = 0.0
    var isFirstInGroup = true

    // Process block of same group, computing differences
    while i < n && groups(i) == g do
      if isFirstInGroup then
        result(i) = values(i) // First element in group gets its own value
        isFirstInGroup = false
      else result(i) = values(i) - prevValue
      end if
      prevValue = values(i)
      i += 1
    end while
  end while

  result
end groupDiff
