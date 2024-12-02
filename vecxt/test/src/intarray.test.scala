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
import vecxt.all.*

class IntArrayExtensionSuite extends munit.FunSuite:
  import BoundsCheck.DoBoundsCheck.yes

  lazy val v_fill = NArray[Double](0, 1, 2, 3, 4)

  test("-") {
    val v1 = NArray.tabulate[Int](21)(i => i)
    val v2 = v1.drop(1) :+ (v1.last + 1)

    assertVecEquals((v1 - v2), NArray.fill[Int](v1.length)(-1))
  }
end IntArrayExtensionSuite
