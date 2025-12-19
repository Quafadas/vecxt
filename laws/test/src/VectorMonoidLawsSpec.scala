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

import cats.kernel.laws.discipline.{MonoidTests, CommutativeMonoidTests}
import cats.kernel.Eq
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Gen}
import vecxt.laws.{Dimension as LawsDimension}
import vecxt.laws.instances.double.*

class VectorMonoidLawsSpec extends DisciplineSuite:
  
  /** Test Monoid laws for a specific dimension */
  def testMonoidLaws(n: Int): Unit =
    // Create dimension witness
    given testDim: LawsDimension = LawsDimension(n)
    
    // Create VectorCommutativeMonoid for this dimension
    given VectorCommutativeMonoid[Double] = 
      vectorAdditionMonoid(using testDim)
    
    // Arbitrary generator for arrays of this dimension
    given Arbitrary[Array[Double]] = Arbitrary(
      Gen.listOfN(n, Arbitrary.arbitrary[Double]).map(_.toArray)
    )
    
    // Equality instance with tolerance for floating point comparisons
    given Eq[Array[Double]] = Eq.instance((a, b) =>
      a.length == b.length && 
      a.zip(b).forall { case (x, y) => Math.abs(x - y) < 1e-6 }
    )
    
    // Test CommutativeMonoid laws (includes all Monoid laws plus commutativity)
    checkAll(
      s"VectorCommutativeMonoid[dim$n, Double].addition",
      CommutativeMonoidTests[Array[Double]].commutativeMonoid
    )
  end testMonoidLaws
  
  // Test various dimensions
  testMonoidLaws(1)
  testMonoidLaws(3)
  testMonoidLaws(10)
  testMonoidLaws(100)
  testMonoidLaws(1000)

end VectorMonoidLawsSpec
