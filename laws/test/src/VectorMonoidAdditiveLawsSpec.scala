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

import cats.kernel.laws.discipline.CommutativeGroupTests
import cats.kernel.Eq
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Gen}
import scala.util.Random
import vecxt.laws.Dimension as LawsDimension
import vecxt.laws.instances.double.*

class VectorGroupLawsSpec extends DisciplineSuite:

  // Test constants for floating-point comparisons
  private val MinTestValue = -100.0
  private val MaxTestValue = 100.0
  private val FloatingPointTolerance = 1e-10

  /** Test Group laws for a specific dimension */
  def testGroupLaws(n: Int): Unit =
    // Create dimension witness
    given testDim: LawsDimension = LawsDimension(n)

    // Create VectorCommutativeGroup for this dimension
    given VectorCommutativeGroup[Double] =
      vectorAdditionGroup(using testDim)

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

    // Test CommutativeGroup laws (includes all Monoid laws plus commutativity and inverse)
    checkAll(
      s"VectorCommutativeGroup[dim$n, Double].addition",
      CommutativeGroupTests[Array[Double]].commutativeGroup
    )
  end testGroupLaws

  // Test various dimensions
  testGroupLaws(0)
  testGroupLaws(1)
  testGroupLaws(3)

  // Test three random dimensions between 5-1000
  private val randomDims = Random.shuffle((5 to 1000).toList).take(2)
  randomDims.foreach(testGroupLaws)

end VectorGroupLawsSpec
