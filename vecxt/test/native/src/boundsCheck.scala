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

import scala.util.chaining.*
import io.gihub.quafadas.vecxt.extensions.*
import io.github.quafadas.vecxt.VectorDimensionMismatch
import io.github.quafadas.vecxt.BoundsCheck


class BoundsCheckSuite extends munit.FunSuite:

  lazy val v_fill = Array.tabulate(5)(i => i.toDouble)

  test("Bounds check") {
    intercept[VectorDimensionMismatch](v_fill.-(Array[Double](1, 2, 3))(using BoundsCheck.yes))
  }

  // test("no bound check") {
  //   intercept[java.lang.ArrayIndexOutOfBoundsException](
  //     v_fill.-(Array[Double](1, 2, 3))(using BoundsChecks.BoundsCheck.no)
  //   )
  // }

end BoundsCheckSuite
