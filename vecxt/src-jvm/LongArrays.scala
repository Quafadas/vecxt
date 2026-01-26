package vecxt

import jdk.incubator.vector.LongVector

object LongArrays:
  final val spl = LongVector.SPECIES_PREFERRED
  final val length = spl.length()

  extension (arr: Array[Long])
    inline def select(indicies: Array[Int]): Array[Long] =
      val len = indicies.length
      val out = Array.ofDim[Long](len)
      var i = 0
      while i < len do
        out(i) = arr(indicies(i))
        i += 1
      end while
      out
    end select

    /** Computes the sum of all elements in the array using SIMD (Single Instruction, Multiple Data) operations.
      *
      * This method leverages the Vector API to perform parallel addition operations on chunks of the array, improving
      * performance for large arrays. The algorithm processes the array in two phases:
      *   1. Vectorized phase: Processes elements in chunks using SIMD instructions up to the loop bound
      *   2. Scalar phase: Processes any remaining elements that don't fit into a complete vector
      *
      * @return
      *   the sum of all elements in the array as a Long value
      */
    inline def sumSIMD: Long =
      val len = arr.length
      var i = 0
      var acc = LongVector.zero(spl)
      val upperBound = spl.loopBound(len)
      while i < upperBound do
        val vec = LongVector.fromArray(spl, arr, i)
        acc = acc.add(vec)
        i += spl.length()
      end while
      var total = acc.reduceLanes(jdk.incubator.vector.VectorOperators.ADD)
      while i < len do
        total += arr(i)
        i += 1
      end while
      total
  end extension

end LongArrays
