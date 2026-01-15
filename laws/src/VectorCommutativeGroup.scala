package vecxt.laws

import cats.kernel.{CommutativeGroup, Semigroup}
import vecxt.BoundsCheck

/** A CommutativeGroup for Array[A] scoped to a specific dimension.
  *
  * This trait extends both VectorMonoid and cats.kernel.CommutativeGroup, making it compatible with cats group laws
  * testing. A CommutativeGroup adds the inverse operation to a CommutativeMonoid.
  *
  * @tparam A
  *   The element type (must form a Semigroup)
  */
trait VectorCommutativeGroup[A] extends VectorMonoid[A] with CommutativeGroup[Array[A]]

object VectorCommutativeGroup:
  /** Summon a VectorCommutativeGroup instance for a specific dimension and type */
  def apply[A](using vcg: VectorCommutativeGroup[A]): VectorCommutativeGroup[A] = vcg

  /** Create a VectorCommutativeGroup instance for a specific dimension
    *
    * @param dim
    *   The dimension for this group
    * @param emptyFn
    *   Function to create the identity element
    * @param combineFn
    *   Function to combine two arrays (must be commutative)
    * @param inverseFn
    *   Function to compute the inverse of an array
    * @param bc
    *   BoundsCheck control for dimension validation
    */
  def forDimension[A: Semigroup](dim: Dimension)(
      emptyFn: => Array[A],
      combineFn: (Array[A], Array[A]) => Array[A],
      inverseFn: Array[A] => Array[A]
  )(using bc: BoundsCheck.BoundsCheck = BoundsCheck.DoBoundsCheck.yes): VectorCommutativeGroup[A] =
    new VectorCommutativeGroup[A]:
      val dimension: Dimension = dim

      def empty = emptyFn

      def combine(x: Array[A], y: Array[A]) =
        if bc == BoundsCheck.DoBoundsCheck.yes then
          validateDim(x)
          validateDim(y)
        end if
        combineFn(x, y)
      end combine

      def inverse(a: Array[A]): Array[A] =
        if bc == BoundsCheck.DoBoundsCheck.yes then validateDim(a)
        end if
        inverseFn(a)
      end inverse
end VectorCommutativeGroup
