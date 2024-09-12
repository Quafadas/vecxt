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

import scala.scalajs.js
import scala.scalajs.js.typedarray.Float64Array
import vecxt.BoundsCheck.BoundsCheck

protected[vecxt] object dimCheck:

  inline def apply[A](a: Float64Array, b: scala.scalajs.js.Array[A])(using inline doCheck: BoundsCheck) =
    inline if doCheck then if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)

  inline def apply(a: Float64Array, b: Float64Array)(using inline doCheck: BoundsCheck) =
    inline if doCheck then if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)
end dimCheck
case class VectorDimensionMismatch(givenDimension: Int, requiredDimension: Int)
    extends Exception(
      s"Expected Vector dimensions to match. First dimension was : $requiredDimension, second was : $givenDimension ."
    )
