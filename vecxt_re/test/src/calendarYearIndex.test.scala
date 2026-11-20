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

  test("explicit currentYear constructor accepts contiguous descending years") {
    val idx = CalendarYearIndex(2024, Array(2024, 2023), Array(1.05, 1.03))

    assertEquals(idx.currentYear, 2024)
    assertEqualsDouble(idx.cumulativeToCurrentFrom(2022), 1.05 * 1.03, 1e-10)
  }

  // --- validation ---

  test("explicit constructor rejects currentYear different from years.head") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(2025, Array(2024, 2023), Array(1.05, 1.03))
    }
  }

  test("explicit constructor rejects ascending years") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(2022, Array(2022, 2023, 2024), Array(1.02, 1.03, 1.05))
    }
  }

  test("explicit constructor rejects gaps in years") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(2024, Array(2024, 2022, 2021), Array(1.05, 1.03, 1.02))
    }
  }

  test("apply rejects gaps in years") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(Array(2024, 2022, 2021), Array(1.05, 1.03, 1.02))
    }
  }

  test("apply rejects duplicate years") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(Array(2024, 2024, 2023), Array(1.05, 1.04, 1.03))
    }
  }

  test("construction rejects zero factor") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(Array(2024, 2023), Array(1.05, 0.0))
    }
  }

  test("construction rejects negative factor") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(Array(2024, 2023), Array(-1.05, 1.03))
    }
  }

  test("fromRateChanges rejects a -100% rate change") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex.fromRateChanges(Array(2024, 2023), Array(5.0, -100.0))
    }
  }

  // --- apply sorts its input ---

  test("apply sorts ascending years into descending order with factors kept aligned") {
    val ascending =
      CalendarYearIndex(Array(2020, 2021, 2022, 2023, 2024, 2025), Array(1.05, 1.03, 1.04, 1.02, 1.06, 1.03))

    assertEquals(ascending.currentYear, 2025)
    assertEquals(ascending.years.toSeq, Seq(2025, 2024, 2023, 2022, 2021, 2020))
    assertEquals(ascending.indices.toSeq, Seq(1.03, 1.06, 1.02, 1.04, 1.03, 1.05))
    assertEquals(ascending.indexAt(2020), 1.05)
    assertEquals(ascending.indexAt(2025), 1.03)
    // 2024 -> 2025 uses only the 2025 factor
    assertEqualsDouble(ascending.cumulativeToCurrentFrom(2024), 1.03, 1e-10)
    assertEqualsDouble(ascending.cumulativeToCurrentFrom(2023), 1.03 * 1.06, 1e-10)
  }

  test("apply gives the same result whatever the input order") {
    val desc = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    val shuffled = CalendarYearIndex(Array(2023, 2022, 2024), Array(1.03, 1.02, 1.05))
    assertEquals(shuffled, desc)
  }

  test("fromRateChanges sorts years with rate changes kept aligned") {
    val idx = CalendarYearIndex.fromRateChanges(Array(2022, 2023, 2024), Array(-2.0, 3.0, 5.0))
    assertEquals(idx.currentYear, 2024)
    assertEqualsDouble(idx.indexAt(2024), 1.05, 1e-10)
    assertEqualsDouble(idx.indexAt(2022), 0.98, 1e-10)
  }

  // --- range handling ---

  test("indexAt throws for future year") {
    val idx = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))
    intercept[NoSuchElementException] {
      idx.indexAt(2025)
    }
  }

  test("earliestOnLevelYear is one before the earliest index year") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    assertEquals(idx.earliestOnLevelYear, 2021)
  }

  test("cumulativeToCurrentFrom throws before earliestOnLevelYear") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    intercept[IllegalArgumentException] {
      idx.cumulativeToCurrentFrom(2020)
    }
    intercept[IllegalArgumentException] {
      idx.cumulativeToCurrentFrom(2015)
    }
  }

  test("cumulativeToCurrentFrom future year returns 1.0") {
    val idx = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))
    assertEquals(idx.cumulativeToCurrentFrom(2026), 1.0)
  }

  test("onLevel throws for data years before the index") {
    val idx = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))
    intercept[IllegalArgumentException] {
      idx.onLevel(Array(100.0, 100.0), Array(2024, 2019))
    }
  }

  test("cumulativeToCurrentFrom with fallback matches strict version inside the index") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    for y <- 2021 to 2025 do
      assertEqualsDouble(idx.cumulativeToCurrentFrom(y, 1.10), idx.cumulativeToCurrentFrom(y), 1e-12)
    end for
  }

  test("cumulativeToCurrentFrom with fallback extrapolates before the index") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    val full = 1.05 * 1.03 * 1.02
    assertEqualsDouble(idx.cumulativeToCurrentFrom(2020, 1.10), full * 1.10, 1e-10)
    assertEqualsDouble(idx.cumulativeToCurrentFrom(2017, 1.10), full * math.pow(1.10, 4), 1e-10)
  }

  test("cumulativeToCurrentFrom rejects non-positive fallback") {
    val idx = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))
    intercept[IllegalArgumentException] {
      idx.cumulativeToCurrentFrom(2015, 0.0)
    }
  }

  test("onLevel with fallback extrapolates before the index") {
    val idx = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))
    val result = idx.onLevel(Array(100.0, 100.0, 100.0), Array(2024, 2022, 2020), 1.10)
    assertEquals(result(0), 100.0)
    assertEqualsDouble(result(1), 100.0 * 1.05 * 1.03, 1e-10)
    assertEqualsDouble(result(2), 100.0 * 1.05 * 1.03 * 1.10 * 1.10, 1e-10)
  }

  // --- single value onLevel ---

  test("onLevel single value in the current year is unchanged") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    assertEquals(idx.onLevel(100.0, 2024), 100.0)
  }

  test("onLevel single value applies cumulative factor to current year") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    assertEqualsDouble(idx.onLevel(100.0, 2023), 100.0 * 1.05, 1e-10)
    assertEqualsDouble(idx.onLevel(100.0, 2021), 100.0 * 1.05 * 1.03 * 1.02, 1e-10)
  }

  test("onLevel single value agrees with the array version") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    val values = Array(100.0, 250.0, 75.0, 10.0)
    val years = Array(2022, 2024, 2021, 2023)
    val arrayResult = idx.onLevel(values, years)
    for i <- values.indices do assertEqualsDouble(idx.onLevel(values(i), years(i)), arrayResult(i), 1e-12)
    end for
  }

  test("onLevel single value future year is unchanged") {
    val idx = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))
    assertEquals(idx.onLevel(100.0, 2026), 100.0)
  }

  test("onLevel single value throws before earliestOnLevelYear") {
    val idx = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))
    intercept[IllegalArgumentException] {
      idx.onLevel(100.0, 2021)
    }
  }

  test("onLevel single value with fallback extrapolates before the index") {
    val idx = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))
    assertEqualsDouble(idx.onLevel(100.0, 2022, 1.10), 100.0 * 1.05 * 1.03, 1e-10)
    assertEqualsDouble(idx.onLevel(100.0, 2020, 1.10), 100.0 * 1.05 * 1.03 * 1.10 * 1.10, 1e-10)
  }

  // --- suggestedNewThreshold ---

  test("suggestedNewThreshold set in the current year is unchanged") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    assertEquals(idx.suggestedNewThreshold(1000.0, 2024), 1000.0)
  }

  test("suggestedNewThreshold on-levels from the year the threshold was set") {
    val idx = CalendarYearIndex(Array(2024, 2023, 2022), Array(1.05, 1.03, 1.02))
    assertEqualsDouble(idx.suggestedNewThreshold(1000.0, 2023), 1000.0 * 1.05, 1e-10)
    assertEqualsDouble(idx.suggestedNewThreshold(1000.0, 2021), 1000.0 * 1.05 * 1.03 * 1.02, 1e-10)
  }

  test("suggestedNewThreshold throws for a threshold year before the index") {
    val idx = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))
    intercept[IllegalArgumentException] {
      idx.suggestedNewThreshold(1000.0, 2010)
    }
  }

  // --- equality ---

  test("equality and hashCode compare array contents") {
    val a = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))
    val b = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))
    assertEquals(a, b)
    assertEquals(a.hashCode, b.hashCode)
  }

  test("equality distinguishes different factors") {
    val a = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))
    val b = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.04))
    assertNotEquals(a, b)
  }

  test("toString shows years and factors") {
    val idx = CalendarYearIndex(Array(2024, 2023), Array(1.05, 1.03))
    assertEquals(idx.toString, "CalendarYearIndex(currentYear=2024, 2024 -> 1.05, 2023 -> 1.03)")
  }

end CalendarYearIndexSuite
