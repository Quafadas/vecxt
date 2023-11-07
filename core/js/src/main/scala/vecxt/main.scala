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

import vecxt.*

import scala.scalajs.js.typedarray.Float64Array
import scala.util.chaining.*

@main def checkBytecode =
  val a = Float64Array(3).tap(_.fill(1.0))
  val a1 = Float64Array(3).tap(_.fill(2.0))
  // val b = Array[Boolean](true, false, true)
  // val c = Array[Boolean](false, true, true)

  import vecxt.BoundsCheck.yes

  a - a1
end checkBytecode
