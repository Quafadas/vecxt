package vecxt_re

import munit.FunSuite

class CalendarYearIndexSuite extends FunSuite:

  test("basic construction with years and indices") {
    val years = Array(2024, 2023, 2022, 2021)
    val indices = Array(1.05, 1.03, 1.02, 1.04)
    val idx = CalendarYearIndex(years, indices)

    assertEquals(idx.currentYear, 2024)
    assertEquals(idx.numYears, 4)
    assertEquals(idx.latestYear, 2024)
    assertEquals(idx.earliestYear, 2021)
  }

  test("indexAt returns correct factor for each year") {
    val years = Array(2024, 2023, 2022)
    val indices = Array(1.05, 1.03, 1.02)
    val idx = CalendarYearIndex(years, indices)

    assertEquals(idx.indexAt(2024), 1.05)
    assertEquals(idx.indexAt(2023), 1.03)
    assertEquals(idx.indexAt(2022), 1.02)
  }

  test("indexAt throws for unknown year") {
    val idx = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))

    intercept[NoSuchElementException] {
      idx.indexAt(2020)
    }
  }

  test("cumulativeToCurrentFrom current year returns 1.0") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    assertEquals(idx.cumulativeToCurrentFrom(2024), 1.0)
  }

  test("cumulativeToCurrentFrom one year back") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    assertEquals(idx.cumulativeToCurrentFrom(2023), 1.05)
  }

  test("cumulativeToCurrentFrom two years back") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    assertEqualsDouble(idx.cumulativeToCurrentFrom(2022), 1.05 * 1.03, 1e-10)
  }

  test("cumulativeToCurrentFrom three years back") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    assertEqualsDouble(idx.cumulativeToCurrentFrom(2021), 1.05 * 1.03 * 1.02, 1e-10)
  }

  test("onLevel applies correct cumulative factors by year") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))

    val values = Array(100.0, 200.0, 300.0)
    val dataYears = Array(2024, 2023, 2022)

    val result = idx.onLevel(values, dataYears)

    assertEquals(result(0), 100.0) // 2024: current year, factor = 1.0
    assertEqualsDouble(result(1), 200.0 * 1.05, 1e-10) // 2023: one year back
    assertEqualsDouble(result(2), 300.0 * 1.05 * 1.03, 1e-10) // 2022: two years back
  }

  test("onLevel with mixed year order") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.10, 1.05, 1.03))

    val values = Array(100.0, 100.0, 100.0)
    val dataYears = Array(2022, 2024, 2023)

    val result = idx.onLevel(values, dataYears)

    assertEqualsDouble(result(0), 100.0 * 1.10 * 1.05, 1e-10) // 2022
    assertEquals(result(1), 100.0) // 2024
    assertEqualsDouble(result(2), 100.0 * 1.10, 1e-10) // 2023
  }

  test("onLevel throws on mismatched array lengths") {
    val idx = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))

    intercept[IllegalArgumentException] {
      idx.onLevel(Array(100.0, 200.0), Array(2024))
    }
  }

  test("constant creates uniform factors across year range") {
    val idx = CalendarYearIndex.constant(2020, 2024, 1.05)

    assertEquals(idx.currentYear, 2024)
    assertEquals(idx.numYears, 5)
    assertEquals(idx.earliestYear, 2020)
    assertEquals(idx.latestYear, 2024)

    assertEquals(idx.indexAt(2024), 1.05)
    assertEquals(idx.indexAt(2023), 1.05)
    assertEquals(idx.indexAt(2020), 1.05)
  }

  test("constant cumulative grows exponentially by years back") {
    val idx = CalendarYearIndex.constant(2020, 2024, 1.10)

    assertEqualsDouble(idx.cumulativeToCurrentFrom(2021), Math.pow(1.10, 3), 1e-10)
  }

  test("fromRateChanges creates correct factors") {
    val years = Array(2024, 2023, 2022)
    val rateChanges = Array(5.0, 3.0, -2.0)
    val idx = CalendarYearIndex.fromRateChanges(years, rateChanges)

    assertEqualsDouble(idx.indexAt(2024), 1.05, 1e-10)
    assertEqualsDouble(idx.indexAt(2023), 1.03, 1e-10)
    assertEqualsDouble(idx.indexAt(2022), 0.98, 1e-10)
  }

  test("construction fails with empty arrays") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(Array.empty[Int], Array.empty[Double])
    }
  }

  test("construction fails with mismatched array lengths") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(2024, Array(2024, 2023), Array(1.05))
    }
  }

  test("explicit currentYear constructor") {
    val idx = CalendarYearIndex(2025, Array(2024, 2023), Array(1.05, 1.03))

    assertEquals(idx.currentYear, 2025)
    // From 2023 to 2025 is 2 years back
    assertEqualsDouble(idx.cumulativeToCurrentFrom(2023), 1.05 * 1.03, 1e-10)
  }

end CalendarYearIndexSuite
