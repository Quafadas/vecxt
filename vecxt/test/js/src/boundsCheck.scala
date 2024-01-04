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

import scala.scalajs.js.typedarray.Float64Array
import scala.util.chaining.*
import io.gihub.quafadas.vecxt.extensions.*
import io.github.quafadas.vecxt.VectorDimensionMismatch

class BoundsCheckSuite extends munit.FunSuite:

  lazy val v_fill = Float64Array(5).tap(a =>
    a(0) = 1.0
    a(1) = 2.0
    a(2) = 3.0
    a(3) = 4.0
    a(4) = 5.0
  )

  test("Bounds check") {
    intercept[VectorDimensionMismatch](v_fill.-(Float64Array(2))(using io.github.quafadas.vecxt.BoundsCheck.yes))
  }

  // I don't know how to do this.
  // test("no bound check") {
  //   intercept[java.lang.ClassCastException](v_fill.-(Float64Array(2))(using vecxt.BoundsCheck.no))
  // }

end BoundsCheckSuite
