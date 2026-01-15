package vecxtensions

import vecxt.reinsurance.Layer


def aggregateByItr(
    years: Array[Int],
    splits: IndexedSeq[(Layer, Array[Double])],
    numItrs: Int
): IndexedSeq[(Layer, Array[Double])] =
  splits.map { case (layer, amounts) => (layer, groupSum(years, amounts, numItrs)) }

end aggregateByItr

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
