package vecxt.laws

import cats.kernel.laws.discipline.CommutativeMonoidTests
import cats.kernel.Eq
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Gen}
import scala.util.Random
import vecxt.laws.Dimension as LawsDimension
import vecxt.laws.instances.double.*

class VectorMultiplicativeMonoidLawsSpec extends DisciplineSuite:

  // Test constants for floating-point comparisons
  // Use smaller range to avoid extreme floating-point precision issues in multiplication
  private val MinTestValue = -10.0
  private val MaxTestValue = 10.0
  // Use more relaxed tolerance for multiplication operations due to accumulated rounding errors
  private val FloatingPointTolerance = 1e-8

  /** Test Monoid laws for a specific dimension */
  def testMonoidLaws(n: Int): Unit =
    // Create dimension witness
    given testDim: LawsDimension = LawsDimension(n)

    // Create VectorCommutativeMonoid for this dimension
    given VectorCommutativeMonoid[Double] =
      vectorMultiplicationMonoid(using testDim)

    // Arbitrary generator for arrays of this dimension
    // Use bounded values to avoid floating point precision issues
    given Arbitrary[Array[Double]] = Arbitrary(
      Gen.listOfN(n, Gen.choose(MinTestValue, MaxTestValue)).map(_.toArray)
    )

    // Equality instance with tolerance for floating point comparisons
    given Eq[Array[Double]] = Eq.instance((a, b) =>
      if a.length != b.length then false
      else
        var i = 0
        var equal = true
        while i < a.length && equal do
          equal = Math.abs(a(i) - b(i)) < FloatingPointTolerance
          i += 1
        end while
        equal
    )

    // Test CommutativeMonoid laws (includes all Monoid laws plus commutativity)
    checkAll(
      s"VectorCommutativeMonoid[dim$n, Double].multiplication",
      CommutativeMonoidTests[Array[Double]].commutativeMonoid
    )
  end testMonoidLaws

  // Test various dimensions
  testMonoidLaws(0)
  testMonoidLaws(1)
  testMonoidLaws(3)

  // Test three random dimensions between 5-1000
  private val randomDims = Random.shuffle((5 to 1000).toList).take(3)
  randomDims.foreach(testMonoidLaws)
end VectorMultiplicativeMonoidLawsSpec
