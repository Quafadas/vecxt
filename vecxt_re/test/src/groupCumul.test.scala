package vecxt_re

import munit.FunSuite

class GroupCumSumSuite extends FunSuite:

  test("basic groupCumSum functionality") {
    val groups = Array(0, 0, 1, 1, 2)
    val values = Array(1.0, 2.0, 3.0, 4.0, 5.0)

    val result = groupCumSum(groups, values)

    assertEquals(result.length, 5)

    // Group 0: cumulative sums [1.0, 3.0]
    assertEquals(result(0), 1.0) // 1.0
    assertEquals(result(1), 3.0) // 1.0 + 2.0

    // Group 1: cumulative sums [3.0, 7.0]
    assertEquals(result(2), 3.0) // 3.0
    assertEquals(result(3), 7.0) // 3.0 + 4.0

    // Group 2: cumulative sum [5.0]
    assertEquals(result(4), 5.0) // 5.0
  }

  test("single group cumulative sum") {
    val groups = Array(0, 0, 0)
    val values = Array(1.0, 2.0, 3.0)

    val result = groupCumSum(groups, values)

    assertEquals(result.length, 3)
    assertEquals(result(0), 1.0) // 1.0
    assertEquals(result(1), 3.0) // 1.0 + 2.0
    assertEquals(result(2), 6.0) // 1.0 + 2.0 + 3.0
  }

  test("single element cumulative sum") {
    val groups = Array(0)
    val values = Array(42.0)

    val result = groupCumSum(groups, values)

    assertEquals(result.length, 1)
    assertEquals(result(0), 42.0)
  }

  test("groups starting from non-zero cumulative sum") {
    val groups = Array(2, 2, 3, 3, 3)
    val values = Array(1.0, 2.0, 3.0, 4.0, 5.0)

    val result = groupCumSum(groups, values)

    assertEquals(result.length, 5)

    // Group 2: cumulative sums [1.0, 3.0]
    assertEquals(result(0), 1.0) // 1.0
    assertEquals(result(1), 3.0) // 1.0 + 2.0

    // Group 3: cumulative sums [3.0, 7.0, 12.0]
    assertEquals(result(2), 3.0) // 3.0
    assertEquals(result(3), 7.0) // 3.0 + 4.0
    assertEquals(result(4), 12.0) // 3.0 + 4.0 + 5.0
  }

  test("negative values cumulative sum") {
    val groups = Array(0, 0, 1, 1)
    val values = Array(-1.0, 2.0, -3.0, 4.0)

    val result = groupCumSum(groups, values)

    assertEquals(result(0), -1.0) // -1.0
    assertEquals(result(1), 1.0) // -1.0 + 2.0
    assertEquals(result(2), -3.0) // -3.0
    assertEquals(result(3), 1.0) // -3.0 + 4.0
  }

  test("zero values cumulative sum") {
    val groups = Array(0, 0, 1, 1)
    val values = Array(0.0, 0.0, 0.0, 5.0)

    val result = groupCumSum(groups, values)

    assertEquals(result(0), 0.0) // 0.0
    assertEquals(result(1), 0.0) // 0.0 + 0.0
    assertEquals(result(2), 0.0) // 0.0
    assertEquals(result(3), 5.0) // 0.0 + 5.0
  }

  test("larger example with gaps cumulative sum") {
    val groups = Array(0, 0, 3, 3, 3, 5, 5)
    val values = Array(1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 3.0)

    val result = groupCumSum(groups, values)

    assertEquals(result.length, 7)

    // Group 0: cumulative sums [1.0, 2.0]
    assertEquals(result(0), 1.0) // 1.0
    assertEquals(result(1), 2.0) // 1.0 + 1.0

    // Group 3: cumulative sums [2.0, 4.0, 6.0]
    assertEquals(result(2), 2.0) // 2.0
    assertEquals(result(3), 4.0) // 2.0 + 2.0
    assertEquals(result(4), 6.0) // 2.0 + 2.0 + 2.0

    // Group 5: cumulative sums [3.0, 6.0]
    assertEquals(result(5), 3.0) // 3.0
    assertEquals(result(6), 6.0) // 3.0 + 3.0
  }

  test("empty array cumulative sum") {
    val groups = Array.empty[Int]
    val values = Array.empty[Double]

    val result = groupCumSum(groups, values)

    assertEquals(result.length, 0)
  }

end GroupCumSumSuite
