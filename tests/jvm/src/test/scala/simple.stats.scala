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

package vecxt

class StatsSuite extends munit.FunSuite:

  test("sample variance and std") {
    val v = Array[Double](2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0)
    assertEqualsDouble(v.variance, 4.571429, 0.00001)
    assertEqualsDouble(v.stdDev, 2.13809, 0.00001)
  }

  test("sample covariance") {
    // Sample version
    // https://corporatefinanceinstitute.com/resources/data-science/covariance/

    val vector1 = Array[Double](1692.0, 1978.0, 1884.0, 2151.0, 2519.0)
    val vector2 = Array[Double](68.0, 102.0, 110.0, 112.0, 154.0)

    val result = vector1.covariance(vector2)

    assertEqualsDouble(result, 9107.3, 0.001)
  }
end StatsSuite
