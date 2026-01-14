/*
 * Copyright 2023 quafadas
 *
 * Licensed under the Apache License, Version 2 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package vecxt


import vecxt.all.*

class BooleaArrayExtensionSuite extends munit.FunSuite:

  test("all") {
    val v1 = Array[Boolean](true, true, true)
    assert(v1.allTrue)

    val v2 = Array[Boolean](true, false, true)
    assert(!v2.allTrue)

    val v3 = Array.fill[Boolean](1025)(true)
    assert(v3.allTrue)
    v3(scala.util.Random.nextInt(1025)) = false
    assert(!v3.allTrue)
  }

  test("any") {
    val v1 = Array[Boolean](false, false, false)
    assert(!v1.any)

    val v2 = Array[Boolean](true, false, true)
    assert(v2.any)

    val v3 = Array.fill[Boolean](1025)(false)
    assert(!v3.any)
    v3(scala.util.Random.nextInt(1025)) = true
    assert(v3.any)
  }

  test("trues") {
    val v1 = Array[Boolean](true, true, true)
    assertEquals(v1.trues, 3)

    val v2 = Array[Boolean](true, false, true)
    assertEquals(v2.trues, 2)

    val v3 = Array.fill[Boolean](1025)(true)
    assertEquals(v3.trues, 1025)
    v3(scala.util.Random.nextInt(1025)) = false
    assertEquals(v3.trues, 1024)
  }

  test("not - small array") {
    val v1 = Array[Boolean](true, false, true, false)
    val result = v1.not

    // Check that result is correct
    assertEquals(result(0), false)
    assertEquals(result(1), true)
    assertEquals(result(2), false)
    assertEquals(result(3), true)

    // Check that original is unchanged
    assertEquals(v1(0), true)
    assertEquals(v1(1), false)
    assertEquals(v1(2), true)
    assertEquals(v1(3), false)
  }

  test("not - large array (tests SIMD path)") {
    // Large enough to trigger SIMD operations (> species length)
    val size = 1025
    val v1 = Array.fill[Boolean](size)(true)
    // Set some to false in a pattern
    (0 until size by 2).foreach(i => v1(i) = false)

    val result = v1.not

    // Verify all values are flipped
    (0 until size).foreach { i =>
      if i % 2 == 0 then assertEquals(result(i), true, s"Expected true at index $i")
      else assertEquals(result(i), false, s"Expected false at index $i")
    }

    // Verify original is unchanged
    (0 until size by 2).foreach(i => assertEquals(v1(i), false))
  }

  test("not - all true") {
    val v1 = Array.fill[Boolean](100)(true)
    val result = v1.not
    assert(result.forall(_ == false), "All values should be false")
    assert(v1.allTrue, "Original should still be all true")
  }

  test("not - all false") {
    val v1 = Array.fill[Boolean](100)(false)
    val result = v1.not
    assert(result.forall(_ == true), "All values should be true")
    assert(!v1.any, "Original should still be all false")
  }

  test("not! - small array") {
    val v1 = Array[Boolean](true, false, true, false)
    v1.`not!`

    assertEquals(v1(0), false)
    assertEquals(v1(1), true)
    assertEquals(v1(2), false)
    assertEquals(v1(3), true)
  }

  test("not! - large array (tests SIMD path)") {
    val size = 1025
    val v1 = Array.fill[Boolean](size)(true)
    (0 until size by 2).foreach(i => v1(i) = false)

    v1.`not!`

    // Verify all values are flipped in place
    (0 until size).foreach { i =>
      if i % 2 == 0 then assertEquals(v1(i), true, s"Expected true at index $i")
      else assertEquals(v1(i), false, s"Expected false at index $i")
    }
  }

  test("not! - all true mutates in place") {
    val v1 = Array.fill[Boolean](100)(true)
    v1.`not!`
    assert(!v1.any, "All values should be false after mutation")
  }

  test("not! - all false mutates in place") {
    val v1 = Array.fill[Boolean](100)(false)
    v1.`not!`
    assert(v1.allTrue, "All values should be true after mutation")
  }

  test("not! - empty array") {
    val v1 = Array.empty[Boolean]
    v1.`not!` // Should not throw
    assertEquals(v1.length, 0)
  }

  test("not - empty array") {
    val v1 = Array.empty[Boolean]
    val result = v1.not
    assertEquals(result.length, 0)
  }

  test("not and not! equivalence") {
    val size = 500
    val v1 = Array.tabulate[Boolean](size)(i => i % 3 == 0)
    val v2 = Array.tabulate[Boolean](size)(i => i % 3 == 0)

    val notResult = v1.not
    v2.`not!`

    (0 until size).foreach { i =>
      assertEquals(notResult(i), v2(i), s"Results should match at index $i")
    }
  }

end BooleaArrayExtensionSuite
