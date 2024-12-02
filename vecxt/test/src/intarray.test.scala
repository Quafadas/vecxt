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

import narr.*
import scala.util.chaining.*
import vecxt.all.*

class IntArrayExtensionSuite extends munit.FunSuite:
  import BoundsCheck.DoBoundsCheck.yes

  lazy val v_fill = NArray[Int](0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

  test("-") {
    val v1 = NArray.tabulate[Int](21)(i => i)
    val v2 = v1.drop(1) :+ (v1.last + 1)

    assertVecEquals((v1 - v2), NArray.fill[Int](v1.length)(-1))
  }

  test("sum") {
    val v1 = NArray.tabulate[Int](10)(i => i)

    assertEquals(v1.sum, 45)
  }

  test("increments") {

    val v1 = NArray[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    v1.increments.foreach(d => assertEquals(d, 1))
    val v2 = NArray[Int](0, 2)
    assertVecEquals(v2.increments, v2)
  }

  test("<") {
    val check = v_fill < 5
    assertVecEquals(check, NArray(true, true, true, true, true, false, false, false, false, false))
  }

end IntArrayExtensionSuite
