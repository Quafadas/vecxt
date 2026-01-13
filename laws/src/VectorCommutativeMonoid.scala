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

package vecxt.laws

import cats.kernel.{CommutativeMonoid, Semigroup}
import vecxt.BoundsCheck

/** A CommutativeMonoid for Array[A] scoped to a specific dimension.
  *
  * This trait extends both VectorMonoid and cats.kernel.CommutativeMonoid, making it compatible with cats commutative
  * monoid laws testing.
  *
  * @tparam A
  *   The element type (must form a Semigroup)
  */
trait VectorCommutativeMonoid[A] extends VectorMonoid[A] with CommutativeMonoid[Array[A]]

object VectorCommutativeMonoid:
  /** Summon a VectorCommutativeMonoid instance for a specific dimension and type */
  def apply[A](using vcm: VectorCommutativeMonoid[A]): VectorCommutativeMonoid[A] = vcm

  /** Create a VectorCommutativeMonoid instance for a specific dimension
    *
    * @param dim
    *   The dimension for this monoid
    * @param emptyFn
    *   Function to create the identity element
    * @param combineFn
    *   Function to combine two arrays (must be commutative)
    * @param bc
    *   BoundsCheck control for dimension validation
    */
  def forDimension[A: Semigroup](dim: Dimension)(
      emptyFn: => Array[A],
      combineFn: (Array[A], Array[A]) => Array[A]
  )(using bc: BoundsCheck.BoundsCheck = BoundsCheck.DoBoundsCheck.yes): VectorCommutativeMonoid[A] =
    new VectorCommutativeMonoid[A]:
      val dimension: Dimension = dim

      def empty = emptyFn

      def combine(x: Array[A], y: Array[A]) =
        if bc == BoundsCheck.DoBoundsCheck.yes then
          validateDim(x)
          validateDim(y)
        end if
        combineFn(x, y)
      end combine
end VectorCommutativeMonoid
