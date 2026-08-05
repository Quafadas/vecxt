package vecxt

import jdk.incubator.vector.DoubleVector

import vecxt.doublearrays.*

/** Phase D — covers the `(from, len)` and `(d, from, len, dest, destFrom)` segment overloads added to
  * `vecxt.doublearrays` so a strided `Matrix[Double]` view can route a contiguous run (a column when `rowStride == 1`,
  * a row when `colStride == 1`) through the same SIMD kernels used by the whole-array,
  * `hasSimpleContiguousMemoryLayout` fast path — without first materialising the segment into its own array.
  *
  * Each test slices out the segment with `.slice`, runs the existing whole-array kernel on the slice as the reference,
  * and compares it against the new segment overload applied directly to the full backing array. Sizes are chosen to
  * straddle a full SIMD species width (`DoubleVector.SPECIES_PREFERRED.length()`) so both the vector body and the
  * scalar tail are exercised.
  */
class DoubleArraysSegmentSuite extends munit.FunSuite:

  val lanes = DoubleVector.SPECIES_PREFERRED.length()
  val segLen = lanes * 2 + 3 // spans at least two full vector iterations plus a scalar tail
  val from = lanes + 1 // a non-zero, non-lane-aligned start offset
  val total = from + segLen + lanes // extra padding on both sides, to catch any out-of-segment writes

  def backing(): Array[Double] = Array.tabulate(total)(i => (i + 1).toDouble)

  test("sumSIMD(from, len) matches whole-array sumSIMD over the equivalent slice") {
    val vec = backing()
    val expected = vec.slice(from, from + segLen).sumSIMD
    assertEqualsDouble(vec.sumSIMD(from, segLen), expected, 1e-9)
  }

  test("norm(from, len) matches whole-array norm over the equivalent slice") {
    val vec = backing()
    val expected = vec.slice(from, from + segLen).norm
    assertEqualsDouble(vec.norm(from, segLen), expected, 1e-9)
  }

  test("multInPlace(d, from, len) scales only the segment, leaving the rest untouched") {
    val vec = backing()
    val before = vec.clone()
    vec.multInPlace(3.0, from, segLen)

    var i = 0
    while i < total do
      val expected = if i >= from && i < from + segLen then before(i) * 3.0 else before(i)
      assertEqualsDouble(vec(i), expected, 1e-9, clue = s"at index $i")
      i += 1
    end while
  }

  test("*=(d, from, len) agrees with multInPlace(d, from, len)") {
    val vecA = backing()
    val vecB = backing()
    vecA.*=(3.5, from, segLen)
    vecB.multInPlace(3.5, from, segLen)
    assertEquals(vecA.toList, vecB.toList)
  }

  test("/=(d, from, len) divides only the segment, leaving the rest untouched") {
    val vec = backing()
    val before = vec.clone()
    vec./=(4.0, from, segLen)

    var i = 0
    while i < total do
      val expected = if i >= from && i < from + segLen then before(i) / 4.0 else before(i)
      assertEqualsDouble(vec(i), expected, 1e-9, clue = s"at index $i")
      i += 1
    end while
  }

  test("+(d, from, len, dest, destFrom) writes d added to the segment into dest at destFrom") {
    val vec = backing()
    val expectedSlice = vec.slice(from, from + segLen).+(2.5)
    val dest = Array.ofDim[Double](segLen)
    vec.+(2.5, from, segLen, dest, 0)
    assertEquals(dest.toList, expectedSlice.toList)
  }

  test("+(d, from, len, dest, destFrom) supports a nonzero destFrom") {
    val vec = backing()
    val expectedSlice = vec.slice(from, from + segLen).+(2.5)
    val dest = Array.ofDim[Double](segLen + 5)
    vec.+(2.5, from, segLen, dest, 5)
    assertEquals(dest.slice(5, 5 + segLen).toList, expectedSlice.toList)
  }

  test("-(d, from, len, dest, destFrom) writes d subtracted from the segment into dest at destFrom") {
    val vec = backing()
    val expectedSlice = vec.slice(from, from + segLen).-(1.25)
    val dest = Array.ofDim[Double](segLen)
    vec.-(1.25, from, segLen, dest, 0)
    assertEquals(dest.toList, expectedSlice.toList)
  }

  test("/(d, from, len, dest, destFrom) writes the segment divided by d into dest at destFrom") {
    val vec = backing()
    val expectedSlice = vec.slice(from, from + segLen)./(3.0)
    val dest = Array.ofDim[Double](segLen)
    vec./(3.0, from, segLen, dest, 0)
    assertEquals(dest.toList, expectedSlice.toList)
  }

  test("segment overloads handle a segment shorter than one SIMD lane") {
    val vec = backing()
    val shortLen = 1
    assertEqualsDouble(vec.sumSIMD(from, shortLen), vec(from), 1e-9)
    val dest = Array.ofDim[Double](shortLen)
    vec.+(1.0, from, shortLen, dest, 0)
    assertEqualsDouble(dest(0), vec(from) + 1.0, 1e-9)
  }
end DoubleArraysSegmentSuite
