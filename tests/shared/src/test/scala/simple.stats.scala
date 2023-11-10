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

import narr.*

class StatsSuite extends munit.FunSuite:

  // import vecxt.BoundsCheck.yes

  test("sample covariance") {
    // Sample version
    // https://corporatefinanceinstitute.com/resources/data-science/covariance/

    val vector1 = NArray[Double](1692.0, 1978.0, 1884.0, 2151.0, 2519.0)
    val vector2 = NArray[Double](68.0, 102.0, 110.0, 112.0, 154.0)

    val result = vector1.covariance(vector2)

    assertEqualsDouble(result, 9107.3, 0.001)
  }

  test("sample variance and std") {
    val v = NArray[Double](2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0)
    assertEqualsDouble(v.variance, 4.571429, 0.00001)
    assertEqualsDouble(v.stdDev, 2.13809, 0.00001)
  }

  test("elementRanks") {

    assertVecEquals(
      NArray.tabulate[Double](10)((i: Int) => 11.0 - i).elementRanks,
      NArray[Double](10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    )
    assertVecEquals(
      NArray.fill[Double](5)(42.0).elementRanks,
      NArray[Double](3, 3, 3, 3, 3)
    )
    assertVecEquals(
      NArray[Double](1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5).elementRanks,
      NArray[Double](1, 2.5, 2.5, 5, 5, 5, 8.5, 8.5, 8.5, 8.5, 13, 13, 13, 13, 13)
    )
    assertVecEquals(
      NArray[Double](1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5).elementRanks,
      NArray[Double](3, 3, 3, 3, 3, 7.5, 7.5, 7.5, 7.5, 11, 11, 11, 13.5, 13.5, 15)
    )
  }

  test("pearson correlation coefficient") {
    // https://www.statisticshowto.com/probability-and-statistics/correlation-coefficient-formula/
    val v1 = NArray[Double](43.0, 21.0, 25.0, 42.0, 57.0, 59.0)
    val v2 = NArray[Double](99.0, 65.0, 79.0, 75.0, 87.0, 81.0)
    assertEqualsDouble(v1.pearsonCorrelationCoefficient(v2)(using vecxt.BoundsCheck.yes), 0.529809, 0.0001)

  }

  test("element rank") {
    val v = NArray[Double](1.0, 5.0, 3.0, 6.0, 1.0, 5.0)
    /*
      1.0 is the first, but has as tied rank. Take the average - 1.5
     */
    assertVecEquals(
      v.elementRanks,
      NArray[Double](1.5, 4.5, 3.0, 6.0, 1.5, 4.5)
    )
  }

  test("spearmans rank") {
    // https://statistics.laerd.com/statistical-guides/spearmans-rank-order-correlation-statistical-guide-2.php
    val v1 = NArray[Double](56.0, 75.0, 45.0, 71.0, 62.0, 64.0, 58.0, 80.0, 76.0, 61.0)
    val v2 = NArray[Double](66.0, 70.0, 40.0, 60.0, 65.0, 56.0, 59.0, 77.0, 67.0, 63.0)
    assertEqualsDouble(v1.spearmansRankCorrelation(v2)(using vecxt.BoundsCheck.yes), 0.6727, 0.001)

    // https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient

    val v3 = NArray[Double](86.0, 97.0, 99.0, 100.0, 101.0, 103.0, 106.0, 110.0, 112.0, 113.0)
    val v4 = NArray[Double](2, 20.0, 28.0, 27.0, 50.0, 29.0, 7.0, 17.0, 6.0, 12.0)
    assertEqualsDouble(-0.1757575, v3.spearmansRankCorrelation(v4)(using vecxt.BoundsCheck.yes), 0.000001)
  }

end StatsSuite
