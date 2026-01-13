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
import vecxt.laws.{Dimension, VectorCommutativeGroup, VectorCommutativeMonoid}
import vecxt.BoundsCheck
import vecxt.all.{given, *}

object double:

  /** VectorCommutativeGroup for Array[Double] with element-wise addition
    *
    * Uses vecxt's optimized array addition operator which leverages SIMD on JVM. Addition forms a group with negation
    * as the inverse operation.
    */
  def vectorAdditionGroup(using dim: Dimension): VectorCommutativeGroup[Double] =
    VectorCommutativeGroup.forDimension(dim)(
      emptyFn = Array.fill(dim.size)(0.0),
      combineFn = (x, y) =>
        import vecxt.BoundsCheck.DoBoundsCheck.yes
        x + y
      ,
      inverseFn = (a) =>
        import vecxt.BoundsCheck.DoBoundsCheck.yes
        -a
    )
  end vectorAdditionGroup

  /** VectorCommutativeMonoid for Array[Double] with element-wise multiplication
    *
    * Note: Element-wise multiplication is commutative but does NOT form a group (no inverse for zero elements). Uses
    * manual implementation for cross-platform compatibility.
    */
  def vectorMultiplicationMonoid(using dim: Dimension): VectorCommutativeMonoid[Double] =
    VectorCommutativeMonoid.forDimension(dim)(
      emptyFn = Array.fill(dim.size)(1.0),
      combineFn = (x, y) => x * y
    )
  end vectorMultiplicationMonoid
end double
