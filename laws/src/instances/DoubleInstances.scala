package vecxt.laws.instances

import vecxt.BoundsCheck
import vecxt.all.{*, given}
import vecxt.laws.Dimension
import vecxt.laws.VectorCommutativeGroup
import vecxt.laws.VectorCommutativeMonoid

import cats.kernel.Semigroup

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
