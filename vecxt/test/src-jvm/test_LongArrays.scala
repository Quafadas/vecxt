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

import vecxt.LongArrays.*

class LongArraysSuite extends munit.FunSuite:

  test("sumSIMD - empty array"):
    val arr = Array.empty[Long]
    assertEquals(arr.sumSIMD, 0L)

  test("sumSIMD - single element"):
    val arr = Array(42L)
    assertEquals(arr.sumSIMD, 42L)

  test("sumSIMD - small array (below SIMD threshold)"):
    val arr = Array(1L, 2L, 3L, 4L, 5L)
    assertEquals(arr.sumSIMD, 15L)

  test("sumSIMD - array exactly matching vector length"):
    val arr = Array.fill(LongArrays.length)(10L)
    assertEquals(arr.sumSIMD, 10L * LongArrays.length)

end LongArraysSuite
