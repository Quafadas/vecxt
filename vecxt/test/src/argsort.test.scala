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

package vecxt

import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

class DoubleArraysSuite extends munit.FunSuite:

  test("unique - empty array"):
    val arr = Array.empty[Double]
    val result = arr.unique
    assertEquals(result.length, 0)

  test("unique - single element"):
    val arr = Array[Double](5.0)
    val result = arr.unique
    assertEquals(result.length, 1)
    assertEquals(result(0), 5.0)

  test("unique - all same elements"):
    val arr = Array[Double](3.0, 3.0, 3.0, 3.0)
    val result = arr.unique
    assertEquals(result.length, 1)
    assertEquals(result(0), 3.0)

  test("unique - already sorted unique elements"):
    val arr = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0)
    val result = arr.unique
    assertEquals(result.length, 5)
    assertVecEquals(result, Array[Double](1.0, 2.0, 3.0, 4.0, 5.0))

  test("unique - unsorted with duplicates"):
    val arr = Array[Double](3.0, 1.0, 4.0, 1.0, 5.0, 9.0, 2.0, 6.0, 5.0)
    val result = arr.unique
    assertEquals(result.length, 7)
    assertVecEquals(result, Array[Double](1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 9.0))

  test("unique - negative numbers"):
    val arr = Array[Double](-3.0, -1.0, -3.0, 0.0, 1.0, -1.0)
    val result = arr.unique
    assertEquals(result.length, 4)
    assertVecEquals(result, Array[Double](-3.0, -1.0, 0.0, 1.0))

  test("unique - with floating point values"):
    val arr = Array[Double](1.5, 2.7, 1.5, 3.14, 2.7)
    val result = arr.unique
    assertEquals(result.length, 3)
    assertVecEquals(result, Array[Double](1.5, 2.7, 3.14))

  test("argsort - empty array"):
    val arr = Array.empty[Double]
    val result = arr.argsort
    assertEquals(result.length, 0)

  test("argsort - single element"):
    val arr = Array[Double](5.0)
    val result = arr.argsort
    assertEquals(result.length, 1)
    assertEquals(result(0), 0)

  test("argsort - already sorted"):
    val arr = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0)
    val result = arr.argsort
    assertVecEquals(result, Array[Int](0, 1, 2, 3, 4))

  test("argsort - reverse sorted"):
    val arr = Array[Double](5.0, 4.0, 3.0, 2.0, 1.0)
    val result = arr.argsort
    assertVecEquals(result, Array[Int](4, 3, 2, 1, 0))

  test("argsort - unsorted array"):
    val arr = Array[Double](3.0, 1.0, 4.0, 1.5, 5.0, 9.0)
    val result = arr.argsort
    assertVecEquals(result, Array[Int](1, 3, 0, 2, 4, 5))
    // Verify that indexing with result gives sorted array
    var i = 0
    var prev = Double.MinValue
    while i < result.length do
      val value = arr(result(i))
      assert(value >= prev)
      prev = value
      i += 1
    end while

  test("argsort - with negative numbers"):
    val arr = Array[Double](-3.0, 5.0, -1.0, 0.0, 2.0)
    val result = arr.argsort
    assertVecEquals(result, Array[Int](0, 2, 3, 4, 1))

  test("argsort - with duplicates"):
    val arr = Array[Double](3.0, 1.0, 3.0, 2.0, 1.0)
    val result = arr.argsort
    // Should maintain stable ordering for equal elements
    assertEquals(result.length, 5)
    var i = 0
    var prev = Double.MinValue
    while i < result.length do
      val value = arr(result(i))
      assert(value >= prev)
      prev = value
      i += 1
    end while

  test("argsort - all elements identical"):
    val arr = Array[Double](1.0, 1.0, 1.0, 1.0, 1.0)
    val result = arr.argsort
    assertVecEquals(result, Array[Int](0, 1, 2, 3, 4)) // stable

  test("argsort - two elements swapped"):
    val arr = Array[Double](1.0, 0.0)
    val result = arr.argsort
    assertVecEquals(result, Array[Int](1, 0))

  test("argsort - alternating high/low"):
    val arr = Array[Double](10, 1, 10, 1, 10, 1)
    val result = arr.argsort
    assertVecEquals(result, Array[Int](1, 3, 5, 0, 2, 4))

  test("argsort - many duplicates, stability check"):
    val arr = Array[Double](2, 3, 2, 3, 2, 3)
    val result = arr.argsort
    // equal 2s keep relative order: 0,2,4
    // equal 3s keep relative order: 1,3,5
    assertVecEquals(result, Array[Int](0, 2, 4, 1, 3, 5))

  test("argsort - array with NaN values (NaN should be last)"):
    val arr = Array[Double](3.0, Double.NaN, 1.0, 2.0)
    val result = arr.argsort
    // Standard rule: NaN compares false for ordering; vec(li) <= vec(ri) fails → NaN goes to end
    assertVecEquals(result, Array[Int](2, 3, 0, 1))

  test("argsort - array with +∞ and -∞"):
    val arr = Array[Double](Double.PositiveInfinity, 5.0, -10.0, Double.NegativeInfinity)
    val result = arr.argsort
    assertVecEquals(result, Array[Int](3, 2, 1, 0))

  test("argsort - insertion cutoff boundary (size = 32)"):
    val size = 32
    val arr = Array.tabulate[Double](size)(i => size - i)
    val result = arr.argsort
    assertVecEquals(result, Array.tabulate[Int](size)(i => size - 1 - i))

  test("argsort - insertion cutoff boundary + 1 (size = 33)"):
    val size = 33
    val arr = Array.tabulate[Double](size)(i => size - i)
    val result = arr.argsort
    assertVecEquals(result, Array.tabulate[Int](size)(i => size - 1 - i))

  test("argsort - random large array (1000 elements)"):
    val rng = scala.util.Random(12345)
    val arr = Array.tabulate[Double](1000)(_ => rng.nextDouble())
    val result = arr.argsort

    // Verify strictly sorted order through indexing
    var i = 1
    while i < result.length do
      assert(arr(result(i - 1)) <= arr(result(i)))
      i += 1
    end while

  test("argsort - pathological merge pattern (descending runs)"):
    // This pattern stresses merge sort structure
    val arr = Array[Double](50.0, 40.0, 30.0, 20.0, 10.0, 0.0)
    val result = arr.argsort
    assertVecEquals(result, Array[Int](5, 4, 3, 2, 1, 0))

  test("argsort - already sorted with duplicates"):
    val arr = Array[Double](1, 1, 2, 2, 3, 3)
    val result = arr.argsort
    assertVecEquals(result, Array[Int](0, 1, 2, 3, 4, 5))

  test("argsort - large group of equal values and one outlier"):
    val arr = Array[Double](Array.fill(100)(5.0)*)
    arr(50) = 0.0 // single minimum
    val result = arr.argsort
    assertEquals(result(0), 50)
    // remaining should be stable 0..49, 51..99
    var i = 1
    var expected = 0
    while i < result.length do
      if expected == 50 then expected = 51
      end if
      assertEquals(result(i), expected)
      expected += 1
      i += 1
    end while

end DoubleArraysSuite
