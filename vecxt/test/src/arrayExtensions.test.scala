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

import narr.*
import scala.util.chaining.*

class ArrayExtensionSuite extends munit.FunSuite:
  import BoundsCheck.DoBoundsCheck.yes


  lazy val v_fill = NArray[Double](0, 1, 2, 3, 4)

  test("Array horizontal sum") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    val v2 = v1 * 2.0
    val v3 = v1 * 3.0

    val l = Array(v1, v2, v3)
    val summed = l.horizontalSum

    assert(summed(0) == 1 + 2 + 3)
    assert(summed(1) == 2 + 4 + 6)
    assert(summed(2) == 3 + 6 + 9)

  }

  test("cumsum") {
    val v1 = NArray[Double](1.0, 2.0, 3.0).tap(_.cumsum)
    assert(v1(0) == 1)
    assert(v1(1) == 3)
    assert(v1(2) == 6)
  }

  test("Array += ") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    v1 += NArray[Double](3.0, 2.0, 1.0)

    assertEqualsDouble(v1(0), 4, 0.00001)
    assertEqualsDouble(v1(1), 4, 0.00001)
    assertEqualsDouble(v1(2), 4, 0.00001)

  }

  test("Array + ") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    val v2 = NArray[Double](3.0, 2.0, 1.0)

    val v3 = v1 + v2

    assertEqualsDouble(v3(0), 4, 0.00001)
    assertEqualsDouble(v3(1), 4, 0.00001)
    assertEqualsDouble(v3(2), 4, 0.00001)

  }

  test("Array -= ") {
    val v1 = NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    v1 -= NArray[Double](3.0, 2.0, 1.0, 0.0, 0.0, 0.0)

    assertEqualsDouble(v1(0), -2, 0.00001)
    assertEqualsDouble(v1(1), 0, 0.00001)
    assertEqualsDouble(v1(2), 2, 0.00001)
    assertEqualsDouble(v1(5), 6, 0.00001)

  }

  test("Array - ") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    val v2 = NArray[Double](3.0, 2.0, 1.0)

    val v3 = v1 - v2

    assertEqualsDouble(v3(0), -2, 0.00001)
    assertEqualsDouble(v3(1), 0, 0.00001)
    assertEqualsDouble(v3(2), 2, 0.00001)
  }

  test("Array *= ") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    v1 *= 2

    assertEqualsDouble(v1(0), 2, 0.00001)
    assertEqualsDouble(v1(1), 4, 0.00001)
    assertEqualsDouble(v1(2), 6, 0.00001)

  }

  test("Array * ") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)

    val v2 = v1 * 2

    assertEqualsDouble(v1(0), 1, 0.00001)
    assertEqualsDouble(v2(1), 4, 0.00001)
    assertEqualsDouble(v2(2), 6, 0.00001)
  }

  test("<=") {
    val v_idx2 = v_fill < 2.5
    assertEquals(v_idx2.countTrue, 3)
  }

  test("<") {
    val v_idx2 = v_fill < 3.0
    assertEquals(v_idx2.countTrue, 3)
  }

  test(">") {
    val v_idx2 = v_fill > 2.5
    assertEquals(v_idx2.countTrue, 2)
  }

  test(">=") {
    val v_idx2 = v_fill >= 3.0
    assertEquals(v_idx2.countTrue, 2)
  }

  test("&&") {
    val v_idx2 = (v_fill < 3.0) && (v_fill > 1.0)
    assertEquals(v_idx2.countTrue, 1)
  }

  test("||") {
    val v_idx2 = (v_fill < 3.0) || (v_fill > 1.0)
    assertEquals(v_idx2.countTrue, 5)

    val v_idx3 = (v_fill < 1.0) || (v_fill > 4.0)
    assertEquals(v_idx3.countTrue, 1)
  }

  test("norm") {
    assertEqualsDouble(v_fill.norm, Math.sqrt(1 + 4 + 9 + 16), 0.00001)
  }

  test("Array indexing") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    val vIdx = NArray[Boolean](true, false, true)
    val afterIndex = v1.idxBoolean(vIdx)

    assertEquals(afterIndex.length, 2)
    assertEqualsDouble(afterIndex.head, 1, 0.0001)
    assertEqualsDouble(afterIndex.last, 3.0, 0.0001)
  }

  // test("max element"){
  //   val v2 = NArray[Double](3.0, 2.0, 4.0, 1.0)
  //   assertEqualsDouble(v2.max, 4.0, 0.00001)
  // }

end ArrayExtensionSuite
