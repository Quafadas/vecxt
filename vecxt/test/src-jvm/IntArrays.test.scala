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

import all.*
import narr.*

class MinMaxSIMDSuite extends munit.FunSuite:

  test("maxSIMD finds maximum in small array"):
    val arr = NArray[Double](1.0, 5.0, 3.0, 9.0, 2.0)
    val result = arr.maxSIMD
    assertEquals(result, 9.0)

  test("maxSIMD finds maximum in large array with SIMD lanes"):
    // Create array larger than SIMD vector length to test SIMD path
    val arr = NArray.tabulate[Double](100)(i => (i * 2.5) % 37.0)
    arr(75) = 1000.0 // Insert known maximum
    val result = arr.maxSIMD
    assertEquals(result, 1000.0)

  test("minSIMD finds minimum in array with positive integers"):
    val arr = Array(5, 3, 9, 1, 7, 2, 8, 4, 6)
    val result = arr.minSIMD
    assertEquals(result, 1)

  test("minSIMD finds minimum in large array crossing SIMD boundaries"):
    // Create array larger than SIMD vector length to test both SIMD and scalar paths
    val arr = Array.tabulate(100)(i => if i == 73 then -5 else i * 2 + 10)
    val result = arr.minSIMD
    assertEquals(result, -5)
end MinMaxSIMDSuite
