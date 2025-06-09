/*
 * Copyright 2023 quafadas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
