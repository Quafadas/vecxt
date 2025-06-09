package vecxtensions

/**   - You have a sorted groups array.
  *     - Each group has a small number of values.
  *   - You’re doing per-group reductions.
  *   - Does not fill in any "gaps" in the groups array
  */
def groupSum(groups: Array[Int], values: Array[Double]): (uniqueGroups: Array[Int], groupSums: Array[Double]) =

  val n = groups.length

  // Count unique groups first
  var uniqueGroupCount = 0
  var i = 0
  while i < n do
    val g = groups(i)
    uniqueGroupCount += 1
    // Skip all elements of the same group
    while i < n && groups(i) == g do i += 1
    end while
  end while

  val uniqueGroups = new Array[Int](uniqueGroupCount)
  val groupSums = new Array[Double](uniqueGroupCount)

  // Fill uniqueGroups with actual group values and compute sums
  var groupIndex = 0
  i = 0
  while i < n do
    val g = groups(i)
    var sum = 0.0
    uniqueGroups(groupIndex) = g

    // process block of same group
    while i < n && groups(i) == g do
      sum += values(i)
      i += 1
    end while

    groupSums(groupIndex) = sum
    groupIndex += 1
  end while

  (uniqueGroups = uniqueGroups, groupSums = groupSums)
end groupSum
