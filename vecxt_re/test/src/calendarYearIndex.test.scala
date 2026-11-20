package vecxt_re

import munit.FunSuite

class CalendarYearIndexSuite extends FunSuite:

  // An index of levels: on-levelling from year y multiplies by level(2024) / level(y)
  private val years = Array(2024, 2023, 2022, 2021)
  private val levels = Array(120.0, 110.0, 105.0, 100.0)
  private def idx = CalendarYearIndex(years, levels)

  test("basic construction with years and index levels") {
    assertEquals(idx.currentYear, 2024)
    assertEquals(idx.numYears, 4)
    assertEquals(idx.latestYear, 2024)
    assertEquals(idx.earliestYear, 2021)
  }

  test("indexAt returns the level for each year") {
    assertEquals(idx.indexAt(2024), 120.0)
    assertEquals(idx.indexAt(2023), 110.0)
    assertEquals(idx.indexAt(2021), 100.0)
  }

  test("indexAt throws for a year before the index") {
    intercept[NoSuchElementException] {
      idx.indexAt(2020)
    }
  }

  test("indexAt throws for a future year") {
    intercept[NoSuchElementException] {
      idx.indexAt(2025)
    }
  }

  // --- onLevelFactor ---

  test("onLevelFactor from the current year is 1.0") {
    assertEquals(idx.onLevelFactor(2024), 1.0)
  }

  test("onLevelFactor is the ratio of current level to the from-year level") {
    assertEqualsDouble(idx.onLevelFactor(2023), 120.0 / 110.0, 1e-12)
    assertEqualsDouble(idx.onLevelFactor(2022), 120.0 / 105.0, 1e-12)
    assertEqualsDouble(idx.onLevelFactor(2021), 120.0 / 100.0, 1e-12)
  }

  test("onLevelFactor is unchanged by rescaling the index") {
    val scaled = CalendarYearIndex(years, levels.map(_ * 3.7e10))
    for y <- 2021 to 2024 do assertEqualsDouble(scaled.onLevelFactor(y), idx.onLevelFactor(y), 1e-12)
    end for
  }

  test("onLevelFactor for a future year is 1.0") {
    assertEquals(idx.onLevelFactor(2026), 1.0)
  }

  test("onLevelFactor throws before the earliest year") {
    intercept[IllegalArgumentException] {
      idx.onLevelFactor(2020)
    }
    intercept[IllegalArgumentException] {
      idx.onLevelFactor(2015)
    }
  }

  test("onLevelFactor with fallback matches the strict version inside the index") {
    for y <- 2021 to 2025 do assertEqualsDouble(idx.onLevelFactor(y, 1.10), idx.onLevelFactor(y), 1e-12)
    end for
  }

  test("onLevelFactor with fallback extrapolates before the earliest year") {
    assertEqualsDouble(idx.onLevelFactor(2020, 1.10), 1.2 * 1.10, 1e-12)
    assertEqualsDouble(idx.onLevelFactor(2017, 1.10), 1.2 * math.pow(1.10, 4), 1e-12)
  }

  test("onLevelFactor rejects a non-positive fallback") {
    intercept[IllegalArgumentException] {
      idx.onLevelFactor(2015, 0.0)
    }
  }

  // --- onLevel, single value ---

  test("onLevel single value in the current year is unchanged") {
    assertEquals(idx.onLevel(value = 100.0, fromYear = 2024), 100.0)
  }

  test("onLevel single value scales by the ratio of levels") {
    assertEqualsDouble(idx.onLevel(value = 1.0, fromYear = 2023), 120.0 / 110.0, 1e-12)
    assertEqualsDouble(idx.onLevel(value = 50.0, fromYear = 2021), 50.0 * 1.2, 1e-12)
  }

  test("onLevel single value future year is unchanged") {
    assertEquals(idx.onLevel(value = 100.0, fromYear = 2026), 100.0)
  }

  test("onLevel single value throws before the earliest year") {
    intercept[IllegalArgumentException] {
      idx.onLevel(value = 100.0, fromYear = 2020)
    }
  }

  test("onLevel single value with fallback extrapolates before the earliest year") {
    assertEqualsDouble(idx.onLevel(value = 1.0, fromYear = 2022, fallbackFactor = 1.10), 120.0 / 105.0, 1e-12)
    assertEqualsDouble(idx.onLevel(value = 1.0, fromYear = 2019, fallbackFactor = 1.10), 1.2 * 1.10 * 1.10, 1e-12)
  }

  test("onLevel single value with named arguments is order independent") {
    assertEqualsDouble(idx.onLevel(fromYear = 2023, value = 1.0), idx.onLevel(value = 1.0, fromYear = 2023), 1e-12)
    assertEqualsDouble(
      idx.onLevel(fallbackFactor = 1.09, fromYear = 2015, value = 1.0),
      idx.onLevel(value = 1.0, fromYear = 2015, fallbackFactor = 1.09),
      1e-12
    )
  }

  // --- onLevel, arrays ---

  test("onLevel array applies the ratio of levels by year, in any order") {
    val result = idx.onLevel(Array(100.0, 200.0, 300.0), Array(2022, 2024, 2023))
    assertEqualsDouble(result(0), 100.0 * 120.0 / 105.0, 1e-10)
    assertEquals(result(1), 200.0)
    assertEqualsDouble(result(2), 300.0 * 120.0 / 110.0, 1e-10)
  }

  test("onLevel array agrees with the single value version") {
    val values = Array(100.0, 250.0, 75.0, 10.0)
    val fromYears = Array(2022, 2024, 2021, 2023)
    val arrayResult = idx.onLevel(values, fromYears)
    for i <- values.indices do
      assertEqualsDouble(idx.onLevel(value = values(i), fromYear = fromYears(i)), arrayResult(i), 1e-12)
    end for
  }

  test("onLevel array throws on mismatched array lengths") {
    intercept[IllegalArgumentException] {
      idx.onLevel(Array(100.0, 200.0), Array(2024))
    }
  }

  test("onLevel array throws for years before the index") {
    intercept[IllegalArgumentException] {
      idx.onLevel(Array(100.0, 100.0), Array(2024, 2019))
    }
  }

  test("onLevel array with fallback extrapolates before the index") {
    val result = idx.onLevel(Array(100.0, 100.0, 100.0), Array(2024, 2021, 2019), 1.10)
    assertEquals(result(0), 100.0)
    assertEqualsDouble(result(1), 100.0 * 1.2, 1e-10)
    assertEqualsDouble(result(2), 100.0 * 1.2 * 1.10 * 1.10, 1e-10)
  }

  // --- suggestedNewThreshold ---

  test("suggestedNewThreshold set in the current year is unchanged") {
    assertEquals(idx.suggestedNewThreshold(1000.0, 2024), 1000.0)
  }

  test("suggestedNewThreshold on-levels from the year the threshold was set") {
    assertEqualsDouble(idx.suggestedNewThreshold(1000.0, 2023), 1000.0 * 120.0 / 110.0, 1e-9)
    assertEqualsDouble(idx.suggestedNewThreshold(1000.0, 2021), 1200.0, 1e-9)
  }

  test("suggestedNewThreshold throws for a threshold year before the index") {
    intercept[IllegalArgumentException] {
      idx.suggestedNewThreshold(1000.0, 2010)
    }
  }

  // --- constant ---

  test("constant grows by the factor each year from 1.0 in fromYear") {
    val c = CalendarYearIndex.constant(2020, 2024, 1.05)
    assertEquals(c.currentYear, 2024)
    assertEquals(c.numYears, 5)
    assertEquals(c.earliestYear, 2020)
    assertEquals(c.indexAt(2020), 1.0)
    assertEqualsDouble(c.indexAt(2024), math.pow(1.05, 4), 1e-12)
  }

  test("constant on-levels by the factor per year") {
    val c = CalendarYearIndex.constant(2020, 2024, 1.10)
    assertEqualsDouble(c.onLevelFactor(2021), math.pow(1.10, 3), 1e-12)
    assertEqualsDouble(c.onLevelFactor(2020), math.pow(1.10, 4), 1e-12)
  }

  // --- fromRateChanges ---

  test("fromRateChanges builds levels with 1.0 in the year before the first change") {
    val r = CalendarYearIndex.fromRateChanges(Array(2024, 2023, 2022), Array(5.0, 3.0, -2.0))
    assertEquals(r.currentYear, 2024)
    assertEquals(r.earliestYear, 2021)
    assertEquals(r.indexAt(2021), 1.0)
    assertEqualsDouble(r.indexAt(2022), 0.98, 1e-12)
    assertEqualsDouble(r.indexAt(2023), 0.98 * 1.03, 1e-12)
    assertEqualsDouble(r.indexAt(2024), 0.98 * 1.03 * 1.05, 1e-12)
  }

  test("fromRateChanges on-levels by the compounded changes after the from year") {
    val r = CalendarYearIndex.fromRateChanges(Array(2024, 2023, 2022), Array(5.0, 3.0, -2.0))
    assertEqualsDouble(r.onLevelFactor(2023), 1.05, 1e-12)
    assertEqualsDouble(r.onLevelFactor(2022), 1.05 * 1.03, 1e-12)
    assertEqualsDouble(r.onLevelFactor(2021), 1.05 * 1.03 * 0.98, 1e-12)
  }

  test("fromRateChanges accepts years in any order") {
    val a = CalendarYearIndex.fromRateChanges(Array(2022, 2024, 2023), Array(-2.0, 5.0, 3.0))
    val b = CalendarYearIndex.fromRateChanges(Array(2024, 2023, 2022), Array(5.0, 3.0, -2.0))
    assertEquals(a, b)
  }

  test("fromRateChanges rejects a -100% rate change") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex.fromRateChanges(Array(2024, 2023), Array(5.0, -100.0))
    }
  }

  test("fromRateChanges rejects gaps in years") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex.fromRateChanges(Array(2024, 2022), Array(5.0, 3.0))
    }
  }

  // --- validation ---

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
    val e = CalendarYearIndex(2024, Array(2024, 2023), Array(110.0, 100.0))
    assertEqualsDouble(e.onLevelFactor(2023), 1.1, 1e-12)
  }

  test("explicit constructor rejects currentYear different from years.head") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(2025, Array(2024, 2023), Array(110.0, 100.0))
    }
  }

  test("explicit constructor rejects ascending years") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(2022, Array(2022, 2023, 2024), Array(100.0, 105.0, 110.0))
    }
  }

  test("explicit constructor rejects gaps in years") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(2024, Array(2024, 2022, 2021), Array(110.0, 105.0, 100.0))
    }
  }

  test("apply rejects gaps in years") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(Array(2024, 2022, 2021), Array(110.0, 105.0, 100.0))
    }
  }

  test("apply rejects duplicate years") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(Array(2024, 2024, 2023), Array(110.0, 109.0, 100.0))
    }
  }

  test("construction rejects a zero level") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(Array(2024, 2023), Array(110.0, 0.0))
    }
  }

  test("construction rejects a negative level") {
    intercept[IllegalArgumentException] {
      CalendarYearIndex(Array(2024, 2023), Array(-110.0, 100.0))
    }
  }

  // --- apply sorts its input ---

  test("apply sorts ascending years into descending order with levels kept aligned") {
    val ascending = CalendarYearIndex(Array(2021, 2022, 2023, 2024), Array(100.0, 105.0, 110.0, 120.0))
    assertEquals(ascending.currentYear, 2024)
    assertEquals(ascending.years.toSeq, Seq(2024, 2023, 2022, 2021))
    assertEquals(ascending.indices.toSeq, Seq(120.0, 110.0, 105.0, 100.0))
    assertEquals(ascending, idx)
  }

  test("apply gives the same result whatever the input order") {
    val shuffled = CalendarYearIndex(Array(2023, 2021, 2024, 2022), Array(110.0, 100.0, 120.0, 105.0))
    assertEquals(shuffled, idx)
  }

  // --- equality ---

  test("equality and hashCode compare array contents") {
    val a = CalendarYearIndex(Array(2024, 2023), Array(110.0, 100.0))
    val b = CalendarYearIndex(Array(2024, 2023), Array(110.0, 100.0))
    assertEquals(a, b)
    assertEquals(a.hashCode, b.hashCode)
  }

  test("equality distinguishes different levels") {
    val a = CalendarYearIndex(Array(2024, 2023), Array(110.0, 100.0))
    val b = CalendarYearIndex(Array(2024, 2023), Array(111.0, 100.0))
    assertNotEquals(a, b)
  }

  test("toString shows years and levels") {
    val a = CalendarYearIndex(Array(2024, 2023), Array(110.0, 100.0))
    assertEquals(a.toString, "CalendarYearIndex(currentYear=2024, 2024 -> 110.0, 2023 -> 100.0)")
  }

end CalendarYearIndexSuite
