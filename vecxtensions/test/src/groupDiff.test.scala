package vecxtensions

import munit.FunSuite

class GroupDiffSuite extends FunSuite:

  test("basic groupDiff functionality") {
    val groups = Array(0, 0, 1, 1, 2)
    val values = Array(1.0, 3.0, 5.0, 8.0, 10.0)

    val result = groupDiff(groups, values)

    assertEquals(result.length, 5)

    // Group 0: differences [1.0, 2.0]
    assertEquals(result(0), 1.0) // First element in group gets its own value
    assertEquals(result(1), 2.0) // 3.0 - 1.0

    // Group 1: differences [5.0, 3.0]
    assertEquals(result(2), 5.0) // First element in group gets its own value
    assertEquals(result(3), 3.0) // 8.0 - 5.0

    // Group 2: differences [10.0]
    assertEquals(result(4), 10.0) // First element in group gets its own value
  }

  test("single group differences") {
    val groups = Array(0, 0, 0)
    val values = Array(1.0, 4.0, 7.0)

    val result = groupDiff(groups, values)

    assertEquals(result.length, 3)
    assertEquals(result(0), 1.0) // First element gets its own value
    assertEquals(result(1), 3.0) // 4.0 - 1.0
    assertEquals(result(2), 3.0) // 7.0 - 4.0
  }

  test("single element differences") {
    val groups = Array(0)
    val values = Array(42.0)

    val result = groupDiff(groups, values)

    assertEquals(result.length, 1)
    assertEquals(result(0), 42.0) // First element in group gets its own value
  }

  test("groups starting from non-zero differences") {
    val groups = Array(2, 2, 3, 3, 3)
    val values = Array(10.0, 15.0, 20.0, 25.0, 30.0)

    val result = groupDiff(groups, values)

    assertEquals(result.length, 5)

    // Group 2: differences [10.0, 5.0]
    assertEquals(result(0), 10.0) // First element in group gets its own value
    assertEquals(result(1), 5.0) // 15.0 - 10.0

    // Group 3: differences [20.0, 5.0, 5.0]
    assertEquals(result(2), 20.0) // First element in group gets its own value
    assertEquals(result(3), 5.0) // 25.0 - 20.0
    assertEquals(result(4), 5.0) // 30.0 - 25.0
  }

  test("negative values differences") {
    val groups = Array(0, 0, 1, 1)
    val values = Array(-10.0, -5.0, -3.0, 2.0)

    val result = groupDiff(groups, values)

    assertEquals(result(0), -10.0) // First element in group gets its own value
    assertEquals(result(1), 5.0) // -5.0 - (-10.0) = 5.0
    assertEquals(result(2), -3.0) // First element in group gets its own value
    assertEquals(result(3), 5.0) // 2.0 - (-3.0) = 5.0
  }

  test("zero values differences") {
    val groups = Array(0, 0, 1, 1)
    val values = Array(0.0, 0.0, 5.0, 5.0)

    val result = groupDiff(groups, values)

    assertEquals(result(0), 0.0) // First element in group gets its own value
    assertEquals(result(1), 0.0) // 0.0 - 0.0 = 0.0
    assertEquals(result(2), 5.0) // First element in group gets its own value
    assertEquals(result(3), 0.0) // 5.0 - 5.0 = 0.0
  }

  test("decreasing values differences") {
    val groups = Array(0, 0, 1, 1)
    val values = Array(10.0, 7.0, 20.0, 15.0)

    val result = groupDiff(groups, values)

    assertEquals(result(0), 10.0) // First element in group gets its own value
    assertEquals(result(1), -3.0) // 7.0 - 10.0 = -3.0
    assertEquals(result(2), 20.0) // First element in group gets its own value
    assertEquals(result(3), -5.0) // 15.0 - 20.0 = -5.0
  }

  test("larger example with gaps differences") {
    val groups = Array(0, 0, 3, 3, 3, 5, 5)
    val values = Array(1.0, 3.0, 10.0, 15.0, 20.0, 100.0, 110.0)

    val result = groupDiff(groups, values)

    assertEquals(result.length, 7)

    // Group 0: differences [1.0, 2.0]
    assertEquals(result(0), 1.0) // First element in group gets its own value
    assertEquals(result(1), 2.0) // 3.0 - 1.0

    // Group 3: differences [10.0, 5.0, 5.0]
    assertEquals(result(2), 10.0) // First element in group gets its own value
    assertEquals(result(3), 5.0) // 15.0 - 10.0
    assertEquals(result(4), 5.0) // 20.0 - 15.0

    // Group 5: differences [100.0, 10.0]
    assertEquals(result(5), 100.0) // First element in group gets its own value
    assertEquals(result(6), 10.0) // 110.0 - 100.0
  }

  test("empty array differences") {
    val groups = Array.empty[Int]
    val values = Array.empty[Double]

    val result = groupDiff(groups, values)

    assertEquals(result.length, 0)
  }

end GroupDiffSuite
