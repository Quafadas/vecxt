package vecxt_re

import vecxt_re.Layer

/** Aggregate each layer's per-event amounts into per-iteration totals.
  *
  * `years(i)` is the 1-based iteration of each layer's `amounts(i)`. It must be sorted in non-decreasing order with
  * values in 1..numItrs, and every `amounts` array must have the same length as `years`. This is NOT checked, for
  * performance: unsorted `years` silently gives wrong totals. See groupSums.scala.
  */
def aggregateByItr(
    years: Array[Int],
    splits: IndexedSeq[(Layer, Array[Double])],
    numItrs: Int
): IndexedSeq[(Layer, Array[Double])] =
  splits.map { case (layer, amounts) => (layer, groupSum(years, amounts, numItrs)) }

end aggregateByItr
