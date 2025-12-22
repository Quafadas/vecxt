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

import cats.kernel.{Monoid, Semigroup}
import vecxt.BoundsCheck

/** A Monoid for Array[A] scoped to a specific dimension.
  *
  * This trait extends cats.kernel.Monoid, making it compatible with the entire cats laws testing infrastructure.
  *
  * @tparam A
  *   The element type (must form a Semigroup)
  */
trait VectorMonoid[A] extends Monoid[Array[A]]:
  /** The dimension this monoid operates on */
  def dimension: Dimension

  /** The identity element - an array of zeros of the correct dimension */
  def empty: Array[A]

  /** Combine two arrays element-wise */
  def combine(x: Array[A], y: Array[A]): Array[A]

  /** Validate that an array has the expected dimension */
  protected def validateDim(arr: Array[A]): Unit =
    if arr.length != dimension.size then
      throw new VectorDimensionException(
        s"Expected dimension ${dimension.size}, got ${arr.length}"
      )
end VectorMonoid

object VectorMonoid:
  /** Summon a VectorMonoid instance for a specific dimension and type */
  def apply[A](using vm: VectorMonoid[A]): VectorMonoid[A] = vm

  /** Create a VectorMonoid instance for a specific dimension
    *
    * @param dim
    *   The dimension for this monoid
    * @param emptyFn
    *   Function to create the identity element
    * @param combineFn
    *   Function to combine two arrays
    * @param bc
    *   BoundsCheck control for dimension validation
    */
  def forDimension[A: Semigroup](dim: Dimension)(
      emptyFn: => Array[A],
      combineFn: (Array[A], Array[A]) => Array[A]
  )(using bc: BoundsCheck.BoundsCheck): VectorMonoid[A] =
    new VectorMonoid[A]:
      val dimension: Dimension = dim

      def empty = emptyFn

      def combine(x: Array[A], y: Array[A]) =
        if bc then          
          validateDim(x)
          validateDim(y)
        end if
        combineFn(x, y)
      end combine
end VectorMonoid
