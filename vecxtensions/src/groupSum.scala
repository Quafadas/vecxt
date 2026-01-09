
package vecxtensions

/**   - You have a sorted groups array.
  *     - Each group has a small number of values.
  *   - You’re doing per-group reductions.
  *   - Does not fill in any "gaps" in the groups array
  */
inline def groupSum(groups: Array[Int], values: Array[Double]): (uniqueGroups: Array[Int], groupSums: Array[Double]) =

  val n = groups.length
  if n == 0 then
    (Array.empty[Int], Array.empty[Double])
  else
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
  end if
end groupSum
