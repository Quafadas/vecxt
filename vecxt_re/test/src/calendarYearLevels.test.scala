package vecxt_re

import munit.FunSuite

class CalendarYearLevelsSuite extends FunSuite:

  // An index of levels: on-levelling from year y multiplies by level(2024) / level(y)
  private val years = Array(2024, 2023, 2022, 2021)
  private val levels = Array(120.0, 110.0, 105.0, 100.0)
  private def idx = CalendarYearLevels(years, levels)

  test("basic construction with years and index levels") {
    assertEquals(idx.currentYear, 2024)
    assertEquals(idx.numYears, 4)
    assertEquals(idx.latestYear, 2024)
    assertEquals(idx.earliestYear, 2021)
  }

  test("levelAt returns the level for each year") {
    assertEquals(idx.levelAt(2024), 120.0)
    assertEquals(idx.levelAt(2023), 110.0)
    assertEquals(idx.levelAt(2021), 100.0)
  }

  test("levelAt throws for a year before the index") {
    intercept[NoSuchElementException] {
      idx.levelAt(2020)
    }
  }

  test("levelAt throws for a future year") {
    intercept[NoSuchElementException] {
      idx.levelAt(2025)
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
    val scaled = CalendarYearLevels(years, levels.map(_ * 3.7e10))
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

  // --- validation ---

  test("construction fails with empty arrays") {
    intercept[IllegalArgumentException] {
      CalendarYearLevels(Array.empty[Int], Array.empty[Double])
    }
  }

  test("construction fails with mismatched array lengths") {
    intercept[IllegalArgumentException] {
      CalendarYearLevels(2024, Array(2024, 2023), Array(1.05))
    }
  }

  test("explicit currentYear constructor accepts contiguous descending years") {
    val e = CalendarYearLevels(2024, Array(2024, 2023), Array(110.0, 100.0))
    assertEqualsDouble(e.onLevelFactor(2023), 1.1, 1e-12)
  }

  test("explicit constructor rejects currentYear different from years.head") {
    intercept[IllegalArgumentException] {
      CalendarYearLevels(2025, Array(2024, 2023), Array(110.0, 100.0))
    }
  }

  test("explicit constructor rejects ascending years") {
    intercept[IllegalArgumentException] {
      CalendarYearLevels(2022, Array(2022, 2023, 2024), Array(100.0, 105.0, 110.0))
    }
  }

  test("explicit constructor rejects gaps in years") {
    intercept[IllegalArgumentException] {
      CalendarYearLevels(2024, Array(2024, 2022, 2021), Array(110.0, 105.0, 100.0))
    }
  }

  test("apply rejects gaps in years") {
    intercept[IllegalArgumentException] {
      CalendarYearLevels(Array(2024, 2022, 2021), Array(110.0, 105.0, 100.0))
    }
  }

  test("apply rejects duplicate years") {
    intercept[IllegalArgumentException] {
      CalendarYearLevels(Array(2024, 2024, 2023), Array(110.0, 109.0, 100.0))
    }
  }

  test("construction rejects a zero level") {
    intercept[IllegalArgumentException] {
      CalendarYearLevels(Array(2024, 2023), Array(110.0, 0.0))
    }
  }

  test("construction rejects a negative level") {
    intercept[IllegalArgumentException] {
      CalendarYearLevels(Array(2024, 2023), Array(-110.0, 100.0))
    }
  }

  // --- apply sorts its input ---

  test("apply sorts ascending years into descending order with levels kept aligned") {
    val ascending = CalendarYearLevels(Array(2021, 2022, 2023, 2024), Array(100.0, 105.0, 110.0, 120.0))
    assertEquals(ascending.currentYear, 2024)
    assertEquals(ascending.years.toSeq, Seq(2024, 2023, 2022, 2021))
    assertEquals(ascending.levels.toSeq, Seq(120.0, 110.0, 105.0, 100.0))
    assertEquals(ascending, idx)
  }

  test("apply gives the same result whatever the input order") {
    val shuffled = CalendarYearLevels(Array(2023, 2021, 2024, 2022), Array(110.0, 100.0, 120.0, 105.0))
    assertEquals(shuffled, idx)
  }

  // --- equality ---

  test("equality and hashCode compare array contents") {
    val a = CalendarYearLevels(Array(2024, 2023), Array(110.0, 100.0))
    val b = CalendarYearLevels(Array(2024, 2023), Array(110.0, 100.0))
    assertEquals(a, b)
    assertEquals(a.hashCode, b.hashCode)
  }

  test("equality distinguishes different levels") {
    val a = CalendarYearLevels(Array(2024, 2023), Array(110.0, 100.0))
    val b = CalendarYearLevels(Array(2024, 2023), Array(111.0, 100.0))
    assertNotEquals(a, b)
  }

  test("toString shows years and levels") {
    val a = CalendarYearLevels(Array(2024, 2023), Array(110.0, 100.0))
    assertEquals(a.toString, "CalendarYearLevels(currentYear=2024, 2024 -> 110.0, 2023 -> 100.0)")
  }

end CalendarYearLevelsSuite
