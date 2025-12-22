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
import vecxt.all.{given, *}

object double:

  /** VectorCommutativeMonoid for Array[Double] with element-wise addition
    *
    * Uses vecxt's optimized array addition operator which leverages SIMD on JVM
    */
  def vectorAdditionMonoid(using dim: Dimension): VectorCommutativeMonoid[Double] =
    VectorCommutativeMonoid.forDimension(dim)(
      emptyFn = Array.fill(dim.size)(0.0),
      combineFn = (x, y) =>
        import vecxt.BoundsCheck.DoBoundsCheck.yes
        x + y
    )
  end vectorAdditionMonoid

  /** VectorCommutativeMonoid for Array[Double] with element-wise multiplication
    *
    * Uses vecxt's optimized array multiplication operator which leverages SIMD on JVM Note: Element-wise multiplication
    * is commutative
    */
  def vectorMultiplicationMonoid(using dim: Dimension): VectorCommutativeMonoid[Double] =
    VectorCommutativeMonoid.forDimension(dim)(
      emptyFn = Array.fill(dim.size)(1.0),
      combineFn = (x, y) =>
        import vecxt.BoundsCheck.DoBoundsCheck.yes
        x * y
    )
  end vectorMultiplicationMonoid
end double
