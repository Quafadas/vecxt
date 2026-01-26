package vecxt_re

import munit.FunSuite

class MixedTest extends FunSuite:

  test("CDF is continuous at mixing point") {
    val values = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val mixingPoint = 4.0
    val paretoShape = 2.0
    val mixed = Mixed.fromValues(values, mixingPoint, paretoShape)

    // CDF just below and at the mixing point should be close
    val cdfBelow = mixed.cdf(mixingPoint - 0.0001)
    val cdfAt = mixed.cdf(mixingPoint)

    // At mixingPoint, the Pareto CDF starts at 0, so cdf should equal bodyWeight
    // which is the fraction of empirical values strictly below mixingPoint
    // Values < 4.0 are: 1.0, 2.0, 3.0 (3 out of 5) = 0.6
    val expectedBodyWeight = 0.6
    assertEqualsDouble(cdfBelow, expectedBodyWeight, 0.01)
    assertEqualsDouble(cdfAt, expectedBodyWeight, 1e-12) // Pareto CDF at scale is 0
  }

  test("CDF goes from 0 to 1") {
    val values = Array(1.0, 2.0, 3.0, 5.0, 10.0)
    val mixingPoint = 4.0
    val paretoShape = 2.0
    val mixed = Mixed.fromValues(values, mixingPoint, paretoShape)

    assertEqualsDouble(mixed.cdf(0.0), 0.0, 1e-12)
    assertEqualsDouble(mixed.cdf(1000000.0), 1.0, 1e-6)
  }

  test("inverseCdf and cdf are consistent") {
    val values = Array(1.0, 2.0, 3.0, 5.0, 10.0)
    val mixingPoint = 4.0
    val paretoShape = 2.5
    val mixed = Mixed.fromValues(values, mixingPoint, paretoShape)

    // Test a range of quantiles
    val quantiles = Array(0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99)
    for p <- quantiles do
      val x = mixed.inverseCdf(p)
      val recoveredP = mixed.cdf(x)
      // For discrete parts, we only expect recoveredP >= p
      assert(recoveredP >= p - 1e-6, s"Failed at p=$p: inverseCdf($p)=$x, cdf($x)=$recoveredP")
    end for
  }

  test("draw returns values in valid range") {
    val values = Array(1.0, 2.0, 3.0)
    val mixingPoint = 2.5
    val paretoShape = 2.0
    val mixed = Mixed.fromValues(values, mixingPoint, paretoShape)

    val samples = (1 to 1000).map(_ => mixed.draw)
    val minSample = samples.min
    val maxSample = samples.max

    // Min should be from empirical (>= 1.0)
    assert(minSample >= 1.0, s"Min sample $minSample should be >= 1.0")
    // Should have some tail samples above mixing point
    assert(maxSample > mixingPoint, s"Max sample $maxSample should be > $mixingPoint (Pareto tail)")
  }

  test("body weight calculation is correct") {
    val values = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    val mixingPoint = 5.5
    val paretoShape = 2.0
    val mixed = Mixed.fromValues(values, mixingPoint, paretoShape)

    // Values < 5.5 are: 1, 2, 3, 4, 5 (5 out of 10) = 0.5
    // CDF at mixing point should equal bodyWeight
    assertEqualsDouble(mixed.cdf(mixingPoint), 0.5, 1e-12)
  }

  test("mean calculation for shape > 1") {
    val values = Array(1.0, 2.0, 3.0)
    val mixingPoint = 2.5
    val paretoShape = 2.5 // Mean is defined for shape > 1
    val mixed = Mixed.fromValues(values, mixingPoint, paretoShape)

    val mean = mixed.mean
    // Mean should be between minimum empirical and some reasonable upper bound
    assert(mean > 1.0, s"Mean $mean should be > 1.0")
    assert(mean.isFinite, s"Mean should be finite")
  }

  test("variance calculation for shape > 2") {
    val values = Array(1.0, 2.0, 3.0, 4.0)
    val mixingPoint = 3.0
    val paretoShape = 3.0 // Variance is defined for shape > 2
    val mixed = Mixed.fromValues(values, mixingPoint, paretoShape)

    val variance = mixed.variance
    assert(variance > 0.0, s"Variance $variance should be > 0")
    assert(variance.isFinite, s"Variance should be finite")
  }

  test("weighted empirical works correctly") {
    val values = Array(1.0, 2.0, 3.0)
    val weights = Array(1.0, 2.0, 1.0) // 25%, 50%, 25%
    val mixingPoint = 2.5
    val paretoShape = 2.0
    val mixed = Mixed.fromWeightedValues(values, weights, mixingPoint, paretoShape)

    // Values < 2.5 are 1.0 and 2.0 with weights 1.0 and 2.0
    // bodyWeight = (1.0 + 2.0) / (1.0 + 2.0 + 1.0) = 0.75
    assertEqualsDouble(mixed.cdf(mixingPoint), 0.75, 1e-12)
  }

  test("probability method works correctly") {
    val values = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val mixingPoint = 3.5
    val paretoShape = 2.0
    val mixed = Mixed.fromValues(values, mixingPoint, paretoShape)

    // P(2 < X <= 3) should be 1/5 = 0.2 (only value 3 is in this range)
    assertEqualsDouble(mixed.probability(2.0, 3.0), 0.2, 1e-12)

    // P(0 < X <= 10) should be close to cdf(10)
    assertEqualsDouble(mixed.probability(0.0, 10.0), mixed.cdf(10.0), 1e-12)
  }

  test("construction fails with non-positive mixing point") {
    intercept[IllegalArgumentException] {
      Mixed.fromValues(Array(1.0, 2.0), 0.0, 2.0)
    }
    intercept[IllegalArgumentException] {
      Mixed.fromValues(Array(1.0, 2.0), -1.0, 2.0)
    }
  }

  test("construction fails with non-positive pareto shape") {
    intercept[IllegalArgumentException] {
      Mixed.fromValues(Array(1.0, 2.0), 1.5, 0.0)
    }
    intercept[IllegalArgumentException] {
      Mixed.fromValues(Array(1.0, 2.0), 1.5, -1.0)
    }
  }

end MixedTest
