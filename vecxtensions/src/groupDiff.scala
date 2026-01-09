package vecxtensions

/**   - You have a sorted groups array.
  *     - Each group has a small number of values.
  *   - You're doing per-group differences.
  *   - Returns differences between consecutive elements within each group
  *   - Each group starts with its first value
  */
inline def groupDiff(groups: Array[Int], values: Array[Double]): Array[Double] =

  val n = groups.length
  if n == 0 then
    Array.empty[Double]
  else
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
  end if
end groupDiff