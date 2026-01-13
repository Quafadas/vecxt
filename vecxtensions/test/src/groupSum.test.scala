package vecxtensions

import munit.FunSuite

class GroupSumSuite extends FunSuite:

  test("basic groupSum functionality") {
    val groups = Array(0, 0, 1, 1, 2)
    val values = Array(1.0, 2.0, 3.0, 4.0, 5.0)

    val (uniqueGroups, groupSums) = groupSum(groups, values)

    assertEquals(uniqueGroups.length, 3)
    assertEquals(groupSums.length, 3)

    assertEquals(uniqueGroups(0), 0)
    assertEquals(uniqueGroups(1), 1)
    assertEquals(uniqueGroups(2), 2)

    assertEquals(groupSums(0), 3.0) // 1.0 + 2.0
    assertEquals(groupSums(1), 7.0) // 3.0 + 4.0
    assertEquals(groupSums(2), 5.0) // 5.0
  }

  test("single group") {
    val groups = Array(0, 0, 0)
    val values = Array(1.0, 2.0, 3.0)

    val (uniqueGroups, groupSums) = groupSum(groups, values)

    assertEquals(uniqueGroups.length, 1)
    assertEquals(groupSums.length, 1)
    assertEquals(uniqueGroups(0), 0)
    assertEquals(groupSums(0), 6.0)
  }

  test("single element") {
    val groups = Array(0)
    val values = Array(42.0)

    val (uniqueGroups, groupSums) = groupSum(groups, values)

    assertEquals(uniqueGroups.length, 1)
    assertEquals(groupSums.length, 1)
    assertEquals(uniqueGroups(0), 0)
    assertEquals(groupSums(0), 42.0)
  }

  test("groups starting from non-zero") {
    val groups = Array(2, 2, 3, 3, 3)
    val values = Array(1.0, 2.0, 3.0, 4.0, 5.0)

    val (uniqueGroups, groupSums) = groupSum(groups, values)

    assertEquals(uniqueGroups.length, 2) // groups 0, 1, 2, 3
    assertEquals(groupSums.length, 2)

    assertEquals(uniqueGroups(0), 2)
    assertEquals(groupSums(0), 3.0) // 1.0 + 2.0

    assertEquals(uniqueGroups(1), 3)
    assertEquals(groupSums(1), 12.0) // 3.0 + 4.0 + 5.0
  }

  test("zero values") {
    val groups = Array(0, 0, 1, 1)
    val values = Array(0.0, 0.0, 0.0, 5.0)

    val (uniqueGroups, groupSums) = groupSum(groups, values)

    assertEquals(groupSums(0), 0.0)
    assertEquals(groupSums(1), 5.0)
  }

  test("larger example with gaps") {
    val groups = Array(0, 0, 3, 3, 3, 5, 5)
    val values = Array(1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 3.0)

    val (uniqueGroups, groupSums) = groupSum(groups, values)

    assertEquals(uniqueGroups.length, 3)
    assertEquals(groupSums.length, 3)

    assertEquals(groupSums(0), 2.0) // 1.0 + 1.0
    assertEquals(groupSums(1), 6.0) // 2.0 + 2.0 + 2.0
    assertEquals(groupSums(2), 6.0) // 3.0 + 3.0

    assertEquals(uniqueGroups(0), 0)
    assertEquals(uniqueGroups(1), 3)
    assertEquals(uniqueGroups(2), 5)
  }

end GroupSumSuite
