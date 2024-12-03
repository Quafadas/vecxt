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

class BooleanArrayExtensionSuite extends munit.FunSuite:
  import BoundsCheck.DoBoundsCheck.yes

  test("all") {
    val v1 = NArray[Boolean](true, true, true)
    assert(v1.all)

    val v2 = NArray[Boolean](true, false, true)
    assert(!v2.all)

    val v3 = NArray.fill[Boolean](1025)(true)
    assert(v3.all)
    v3(scala.util.Random.nextInt(1025)) = false
    assert(!v3.all)
  }

  test("any") {
    val v1 = NArray[Boolean](false, false, false)
    assert(v1.any)

    val v2 = NArray[Boolean](true, false, true)
    assert(v2.any)

    val v3 = NArray.fill[Boolean](1025)(false)
    assert(!v3.any)
    v3(scala.util.Random.nextInt(1025)) = true
    assert(v3.any)
  }

  test("trues") {
    val v1 = NArray[Boolean](true, true, true)
    assertEquals(v1.trues, 3)

    val v2 = NArray[Boolean](true, false, true)
    assertEquals(v2.trues, 2)

    val v3 = NArray.fill[Boolean](1025)(true)
    assertEquals(v3.trues, 1025)
    v3(scala.util.Random.nextInt(1025)) = false
    assertEquals(v3.trues, 1024)
  }

end BooleanArrayExtensionSuite
