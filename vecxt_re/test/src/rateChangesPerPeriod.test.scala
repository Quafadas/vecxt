package vecxt_re

import munit.FunSuite

class RateChangesPerPeriodSuite extends FunSuite:

  test("factorAt returns correct index for each period") {
    val factors = Array(1.05, 1.03, 1.02)
    val idx = RateChangesPerPeriod(factors)

    assertEquals(idx.factorAt(0), 1.05)
    assertEquals(idx.factorAt(1), 1.03)
    assertEquals(idx.factorAt(2), 1.02)
  }

  test("numPeriods returns correct count") {
    val idx = RateChangesPerPeriod(Array(1.05, 1.03, 1.02))
    assertEquals(idx.numPeriods, 3)
  }

  test("cumulativeToCurrentFrom period 0 returns 1.0") {
    val idx = RateChangesPerPeriod(Array(1.05, 1.03, 1.02))
    assertEquals(idx.cumulativeToCurrentFrom(0), 1.0)
  }

  test("cumulativeToCurrentFrom negative period returns 1.0") {
    val idx = RateChangesPerPeriod(Array(1.05, 1.03, 1.02))
    assertEquals(idx.cumulativeToCurrentFrom(-1), 1.0)
  }

  test("cumulativeToCurrentFrom period 1 returns first index") {
    val idx = RateChangesPerPeriod(Array(1.05, 1.03, 1.02))
    assertEquals(idx.cumulativeToCurrentFrom(1), 1.05)
  }

  test("cumulativeToCurrentFrom period 2 returns product of first two factors") {
    val idx = RateChangesPerPeriod(Array(1.05, 1.03, 1.02))
    assertEqualsDouble(idx.cumulativeToCurrentFrom(2), 1.05 * 1.03, 1e-10)
  }

  test("cumulativeToCurrentFrom period 3 returns product of all factors") {
    val idx = RateChangesPerPeriod(Array(1.05, 1.03, 1.02))
    assertEqualsDouble(idx.cumulativeToCurrentFrom(3), 1.05 * 1.03 * 1.02, 1e-10)
  }

  test("cumulativeToCurrentFrom beyond available periods uses all factors") {
    val idx = RateChangesPerPeriod(Array(1.05, 1.03))
    assertEqualsDouble(idx.cumulativeToCurrentFrom(5), 1.05 * 1.03, 1e-10)
  }

  test("cumulativeFactors returns correct array") {
    val idx = RateChangesPerPeriod(Array(1.05, 1.03, 1.02))
    val factors = idx.cumulativeFactors(4)

    assertEquals(factors.length, 4)
    assertEquals(factors(0), 1.0)
    assertEqualsDouble(factors(1), 1.05, 1e-10)
    assertEqualsDouble(factors(2), 1.05 * 1.03, 1e-10)
    assertEqualsDouble(factors(3), 1.05 * 1.03 * 1.02, 1e-10)
  }

  test("cumulativeFactors with upToPeriod less than numPeriods") {
    val idx = RateChangesPerPeriod(Array(1.05, 1.03, 1.02))
    val factors = idx.cumulativeFactors(2)

    assertEquals(factors.length, 2)
    assertEquals(factors(0), 1.0)
    assertEqualsDouble(factors(1), 1.05, 1e-10)
  }

  test("onLevel applies correct cumulative factors") {
    val idx = RateChangesPerPeriod(Array(1.05, 1.03, 1.02))

    val values = Array(100.0, 200.0, 300.0)
    val periods = Array(0, 1, 2)

    val result = idx.onLevel(values, periods)

    assertEquals(result.length, 3)
    assertEquals(result(0), 100.0) // period 0: * 1.0
    assertEqualsDouble(result(1), 200.0 * 1.05, 1e-10) // period 1: * 1.05
    assertEqualsDouble(result(2), 300.0 * 1.05 * 1.03, 1e-10) // period 2: * (1.05 * 1.03)
  }

  test("onLevel with mixed period order") {
    val idx = RateChangesPerPeriod(Array(1.10, 1.05))

    val values = Array(100.0, 100.0, 100.0)
    val periods = Array(2, 0, 1)

    val result = idx.onLevel(values, periods)

    assertEqualsDouble(result(0), 100.0 * 1.10 * 1.05, 1e-10) // period 2
    assertEquals(result(1), 100.0) // period 0
    assertEqualsDouble(result(2), 100.0 * 1.10, 1e-10) // period 1
  }

  test("onLevel throws on mismatched array lengths") {
    val idx = RateChangesPerPeriod(Array(1.05))
    val values = Array(100.0, 200.0)
    val periods = Array(0)

    intercept[IllegalArgumentException] {
      idx.onLevel(values, periods)
    }
  }

  test("fromRateChanges creates correct factors") {
    val idx = RateChangesPerPeriod.fromRateChanges(Array(5.0, 3.0, -2.0))

    assertEqualsDouble(idx.factorAt(0), 1.05, 1e-10)
    assertEqualsDouble(idx.factorAt(1), 1.03, 1e-10)
    assertEqualsDouble(idx.factorAt(2), 0.98, 1e-10)
  }

  test("constant creates uniform factors") {
    val idx = RateChangesPerPeriod.constant(3, 1.05)

    assertEquals(idx.numPeriods, 3)
    assertEquals(idx.factorAt(0), 1.05)
    assertEquals(idx.factorAt(1), 1.05)
    assertEquals(idx.factorAt(2), 1.05)
  }

  test("constant cumulative grows exponentially") {
    val idx = RateChangesPerPeriod.constant(5, 1.10)

    assertEqualsDouble(idx.cumulativeToCurrentFrom(3), Math.pow(1.10, 3), 1e-10)
  }

end RateChangesPerPeriodSuite
