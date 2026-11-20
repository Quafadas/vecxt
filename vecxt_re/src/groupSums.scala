package vecxt_re

/* Group-by-iteration kernels.
 *
 * These are on the hot path of every loss report and scenario statistic: they run per layer, over arrays with one
 * entry per simulated event (often millions). They are deliberately UNCHECKED. Verifying the preconditions below would
 * need an extra full pass over `groups` on every call, roughly doubling the memory traffic of a single-pass kernel, to
 * re-prove something the data producers already guarantee. The producers that establish the invariant are
 * `Scenario` (sorts on construction), `Scenarr.sorted`, `Scenarr.combine` and `FreqSev.drawScenarr`. Public entry
 * points that take a raw `years` / `iterations` array from users document the same precondition.
 *
 * Shared preconditions (the caller's responsibility):
 *   - `groups.length == values.length` (where `values` is taken)
 *   - `groups` is sorted in non-decreasing order, so equal indices form a single contiguous run
 *   - every index g satisfies 1 <= g <= nitr (where `nitr` is taken)
 *
 * If they are violated the result is silently wrong rather than an error:
 *   - unsorted `groups`: each run of equal indices OVERWRITES the slot for that group, so a group split across several
 *     runs keeps only its last run. e.g. groupSum([1, 2, 1], [1, 1, 1], 2) == [1, 1], not [2, 1]
 *   - `values` longer than `groups`: the extra values are ignored. Shorter: ArrayIndexOutOfBoundsException
 *   - an index outside 1..nitr: ArrayIndexOutOfBoundsException on the JVM and Native. On Scala.js, out-of-bounds
 *     array access is unchecked in optimised builds, so the behaviour is undefined
 *
 * These are package-private so that the only callers are ones that maintain the invariant.
 */

/** Cumulative sum of `values` within each run of equal `groups`, element by element.
  *
  * Preconditions: see the file header (`nitr` does not apply). They are not checked, for performance. With unsorted
  * `groups` the running total restarts at every new run, so a group split across runs gets a per-run cumulative sum.
  *
  * Example: groups = [1, 1, 2, 2, 2], values = [1, 2, 3, 4, 5] => [1, 3, 3, 7, 12]
  */
private[vecxt_re] def groupCumSum(groups: Array[Int], values: Array[Double]): Array[Double] =

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

/** Sum of values for each 1-based group index.
  *
  * Returns an array of length `nitr`. Slot `i` (0-based) holds the sum for group `i + 1`; groups with no entries are
  * 0.0.
  *
  * Preconditions and failure modes: see the file header. They are not checked, for performance.
  *
  * Complexity: one pass over `groups`, O(nitr) extra space.
  *
  * Example: groups = [1, 1, 3], values = [1.0, 2.0, 4.0], nitr = 4 => [3.0, 0.0, 4.0, 0.0]
  */
private[vecxt_re] def groupSum(groups: Array[Int], values: Array[Double], nitr: Int): Array[Double] =
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

/** Number of entries for each 1-based group index.
  *
  * Returns an array of length `nitr`. Slot `i` (0-based) holds the count for group `i + 1`; groups with no entries are
  * 0.
  *
  * Preconditions and failure modes: see the file header. They are not checked, for performance.
  *
  * Example: groups = [1, 1, 3], nitr = 4 => [2, 0, 1, 0]
  */
private[vecxt_re] def groupCount(groups: Array[Int], nitr: Int): Array[Int] =
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

/** Maximum of values for each 1-based group index.
  *
  * Returns an array of length `nitr`. Slot `i` (0-based) holds the max for group `i + 1`; groups with no entries are
  * Double.NegativeInfinity.
  *
  * Preconditions and failure modes: see the file header. They are not checked, for performance.
  *
  * Complexity: one pass over `groups`, O(nitr) extra space.
  *
  * Example: groups = [1, 1, 3], values = [1.0, 2.0, 4.0], nitr = 4 => [2.0, -Inf, 4.0, -Inf]
  */
private[vecxt_re] def groupMax(groups: Array[Int], values: Array[Double], nitr: Int): Array[Double] =
  val result = Array.fill(nitr)(Double.NegativeInfinity)
  val l = groups.length
  var i = 0
  while i < l do
    val g = groups(i)
    var groupMax = Double.NegativeInfinity
    // Process block of same group, computing max
    while i < l && groups(i) == g do
      if values(i) > groupMax then groupMax = values(i)
      end if
      i += 1
    end while
    result(g - 1) = groupMax
  end while

  result
end groupMax
