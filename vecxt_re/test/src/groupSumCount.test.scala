package vecxt_re

import munit.FunSuite

class GroupSumCountSuite extends FunSuite:

  test("groupSum aggregates per 1-based group index with gaps") {
    val groups = Array(1, 1, 2, 4, 4)
    val values = Array(2.0, 3.0, 5, 10, 20)

    val result = groupSum(groups, values, nitr = 4)

    assertEquals(result.length, 4)
    assertVecEquals(result, Array(5.0, 5, 0, 30))
  }

  test("groupCount counts occurrences per group index") {
    val groups = Array(1, 1, 2, 4, 4)

    val result = groupCount(groups, nitr = 4)

    assertEquals(result.length, 4)
    assertVecEquals(result, Array(2, 1, 0, 2))
  }

  test("handles empty input by returning zeroed buckets") {
    val groups = Array.empty[Int]
    val values = Array.empty[Double]

    val sumResult = groupSum(groups, values, nitr = 3)
    val countResult = groupCount(groups, nitr = 3)

    assertEquals(sumResult.length, 3)
    assertEquals(countResult.length, 3)
    assertVecEquals(sumResult, Array(0.0, 0, 0))
    assertVecEquals(countResult, Array(0, 0, 0))
  }

  test("single group spanning all entries") {
    val groups = Array(3, 3, 3)
    val values = Array(1.5, 2.5, -4)

    val sumResult = groupSum(groups, values, nitr = 4)
    val countResult = groupCount(groups, nitr = 4)

    val expectedSum = Array(0, 0, values.sum, 0)
    val expectedCount = Array(0, 0, 3, 0)

    assertVecEquals(sumResult, expectedSum)
    assertVecEquals(countResult, expectedCount)
  }

  test("groupMax finds max per 1-based group index with gaps") {
    val groups = Array(1, 1, 2, 4, 4)
    val values = Array(2.0, 3.0, 5.0, 10.0, 20.0)

    val result = groupMax(groups, values, nitr = 4)

    assertEquals(result.length, 4)
    assertVecEquals(result, Array(3.0, 5.0, Double.NegativeInfinity, 20.0))
  }

  test("groupMax handles empty input by returning -Inf buckets") {
    val groups = Array.empty[Int]
    val values = Array.empty[Double]

    val maxResult = groupMax(groups, values, nitr = 3)

    assertEquals(maxResult.length, 3)
    assert(maxResult.forall(_ == Double.NegativeInfinity))
  }

  test("groupMax single group spanning all entries") {
    val groups = Array(3, 3, 3)
    val values = Array(1.5, 2.5, -4.0)

    val maxResult = groupMax(groups, values, nitr = 4)

    val expectedMax = Array(Double.NegativeInfinity, Double.NegativeInfinity, 2.5, Double.NegativeInfinity)
    assertVecEquals(maxResult, expectedMax)
  }

  test("groupMax with negative values") {
    val groups = Array(1, 1, 2, 2)
    val values = Array(-5.0, -2.0, -10.0, -3.0)

    val result = groupMax(groups, values, nitr = 2)

    assertVecEquals(result, Array(-2.0, -3.0))
  }
end GroupSumCountSuite
