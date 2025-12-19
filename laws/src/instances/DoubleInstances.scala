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

package vecxt.laws.instances

import cats.kernel.Semigroup
import vecxt.laws.{Dimension, VectorCommutativeMonoid}
import vecxt.BoundsCheck

object double:
  // Semigroup instances for Double operations
  given additionSemigroup: Semigroup[Double] = Semigroup.instance[Double](_ + _)
  given multiplicationSemigroup: Semigroup[Double] = Semigroup.instance[Double](_ * _)

  /** VectorCommutativeMonoid for Array[Double] with element-wise addition */
  def vectorAdditionMonoid(using dim: Dimension): VectorCommutativeMonoid[Double] =
    VectorCommutativeMonoid.forDimension(dim)(
      emptyFn = Array.fill(dim.size)(0.0),
      combineFn = (x, y) =>
        val result = new Array[Double](x.length)
        var i = 0
        while i < x.length do
          result(i) = x(i) + y(i)
          i += 1
        end while
        result
    )(using additionSemigroup, BoundsCheck.DoBoundsCheck.yes)
  end vectorAdditionMonoid

  /** VectorCommutativeMonoid for Array[Double] with element-wise multiplication
    *
    * Note: Element-wise multiplication is commutative
    */
  def vectorMultiplicationMonoid(using dim: Dimension): VectorCommutativeMonoid[Double] =
    VectorCommutativeMonoid.forDimension(dim)(
      emptyFn = Array.fill(dim.size)(1.0),
      combineFn = (x, y) =>
        val result = new Array[Double](x.length)
        var i = 0
        while i < x.length do
          result(i) = x(i) * y(i)
          i += 1
        end while
        result
    )(using multiplicationSemigroup, BoundsCheck.DoBoundsCheck.yes)
  end vectorMultiplicationMonoid
end double
