package vecxt_re

import munit.FunSuite

class EmpiricalTest extends FunSuite:

  test("cdf/inverseCdf are consistent (unweighted)") {
    val xs = Array(3.0, 1.0, 2.0, 2.0)
    val emp = Empirical.equalWeights(xs)

    assertEqualsDouble(emp.cdf(0.5), 0.0, 1e-12)
    assertEqualsDouble(emp.cdf(1.0), 0.25, 1e-12)
    assertEqualsDouble(emp.cdf(1.5), 0.25, 1e-12)
    assertEqualsDouble(emp.cdf(2.0), 0.75, 1e-12)
    assertEqualsDouble(emp.cdf(10.0), 1.0, 1e-12)

    // Quantiles: smallest x with F(x) >= p
    assertEqualsDouble(emp.inverseCdf(0.0), 1.0, 1e-12)
    assertEqualsDouble(emp.inverseCdf(0.25), 1.0, 1e-12)
    assertEqualsDouble(emp.inverseCdf(0.2500001), 2.0, 1e-12)
    assertEqualsDouble(emp.inverseCdf(0.75), 2.0, 1e-12)
    assertEqualsDouble(emp.inverseCdf(1.0), 3.0, 1e-12)
  }

  test("mean/variance match weighted formulas") {
    val xs = Array(1.0, 10.0)
    val ws = Array(3.0, 1.0) // 75% at 1, 25% at 10
    val emp = Empirical.weighted(xs, ws)

    val mean = 0.75 * 1.0 + 0.25 * 10.0
    val variance = 0.75 * (1.0 - mean) * (1.0 - mean) + 0.25 * (10.0 - mean) * (10.0 - mean)

    assertEqualsDouble(emp.mean, mean, 1e-12)
    assertEqualsDouble(emp.variance, variance, 1e-12)

    assertEqualsDouble(emp.probabilityOf(1.0), 0.75, 1e-12)
    assertEqualsDouble(emp.probabilityOf(10.0), 0.25, 1e-12)
    assertEqualsDouble(emp.probability(1.0, 10.0), 0.25, 1e-12) // P(1 < X <= 10) = 0.25
  }

  test("single element distribution") {
    val emp = Empirical.equalWeights(Array(42.0))
    assertEqualsDouble(emp.mean, 42.0, 1e-12)
    assertEqualsDouble(emp.variance, 0.0, 1e-12)
    assertEqualsDouble(emp.cdf(41.0), 0.0, 1e-12)
    assertEqualsDouble(emp.cdf(42.0), 1.0, 1e-12)
    assertEqualsDouble(emp.inverseCdf(0.0), 42.0, 1e-12)
    assertEqualsDouble(emp.inverseCdf(0.5), 42.0, 1e-12)
    assertEqualsDouble(emp.inverseCdf(1.0), 42.0, 1e-12)
    assertEqualsDouble(emp.probabilityOf(42.0), 1.0, 1e-12)
  }

  test("all duplicate values are merged") {
    val emp = Empirical.equalWeights(Array(5.0, 5.0, 5.0))
    assertEqualsDouble(emp.mean, 5.0, 1e-12)
    assertEqualsDouble(emp.variance, 0.0, 1e-12)
    assertEqualsDouble(emp.probabilityOf(5.0), 1.0, 1e-12)
  }

  test("inverseCdf(1.0) returns maximum") {
    val emp = Empirical.equalWeights(Array(1.0, 2.0, 100.0))
    assertEqualsDouble(emp.inverseCdf(1.0), 100.0, 1e-12)
  }

  test("cdf at exact max value equals 1") {
    val emp = Empirical.equalWeights(Array(1.0, 2.0, 3.0))
    assertEqualsDouble(emp.cdf(3.0), 1.0, 1e-12)
  }

  test("draw returns values in support") {
    val xs = Array(10.0, 20.0, 30.0)
    val emp = Empirical.equalWeights(xs)
    val samples = (1 to 100).map(_ => emp.draw)
    assert(samples.forall(s => xs.contains(s)))
  }

  test("construction fails on empty values") {
    intercept[IllegalArgumentException] {
      Empirical.equalWeights(Array.empty[Double])
    }
  }

  test("construction fails on zero weight") {
    intercept[IllegalArgumentException] {
      Empirical.weighted(Array(1.0), Array(0.0))
    }
  }

  test("construction fails on negative weight") {
    intercept[IllegalArgumentException] {
      Empirical.weighted(Array(1.0, 2.0), Array(1.0, -1.0))
    }
  }

  test("construction fails on NaN in values") {
    intercept[IllegalArgumentException] {
      Empirical.equalWeights(Array(1.0, Double.NaN))
    }
  }

end EmpiricalTest
