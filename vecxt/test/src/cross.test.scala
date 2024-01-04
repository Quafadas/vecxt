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

package io.gihub.quafadas.vecxt

import narr.*

import scala.util.chaining.*
import io.gihub.quafadas.vecxt.extensions.*

class XSuite extends munit.FunSuite:

  // This test is a duplicate ... but if it works, it proves that the extension methods work on every platform with a common NArray supertype :-)...
  test("reinsurance function - ret and limit") {
    val v = NArray
      .ofSize[Double](3)
      .tap(n =>
        n(0) = 8
        n(1) = 11
        n(2) = 16
      )
    v.reinsuranceFunction(Some(5.0), Some(10.0))
    assert(v(0) == 0.0)
    assert(v(1) == 1.0)
    assert(v(2) == 5.0)
  }
end XSuite
