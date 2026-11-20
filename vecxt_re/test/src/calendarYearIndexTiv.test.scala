package vecxt_re

import munit.FunSuite

/** Regression test with a real TIV level series, passed to the index directly. */
class CalendarYearIndexTivSuite extends FunSuite:

  private val indexRaw = Seq(
    (year = 2026, tiv = 3803475174199d),
    (year = 2025, tiv = 3695685309234d),
    (year = 2024, tiv = 3413983612594d),
    (year = 2023, tiv = 3129579712001d),
    (year = 2022, tiv = 2784848722341d),
    (year = 2021, tiv = 2442947161963d),
    (year = 2020, tiv = 2170093969728d),
    (year = 2019, tiv = 2029813961272d)
  )

  private val cyi = CalendarYearIndex(indexRaw.map(_.year).toArray, indexRaw.map(_.tiv).toArray)

  test("on-level 1 from each year is tiv(2026) / tiv(year)") {
    for r <- indexRaw do
      assertEqualsDouble(cyi.onLevel(value = 1.0, fromYear = r.year), 3803475174199d / r.tiv, 1e-12, s"year ${r.year}")
    end for
  }

  test("on-level factors are of order 1, not of order the TIV") {
    assertEqualsDouble(cyi.onLevel(value = 1.0, fromYear = 2025), 1.02917, 1e-5)
    assertEqualsDouble(cyi.onLevel(value = 1.0, fromYear = 2024), 1.11409, 1e-5)
    assertEqualsDouble(cyi.onLevel(value = 1.0, fromYear = 2023), 1.21533, 1e-5)
    assertEqualsDouble(cyi.onLevel(value = 1.0, fromYear = 2019), 1.87380, 1e-5)
  }

  test("fallback extends the index before 2019") {
    assertEqualsDouble(
      cyi.onLevel(value = 1.0, fromYear = 2017, fallbackFactor = 1.09),
      3803475174199d / 2029813961272d * 1.09 * 1.09,
      1e-12
    )
  }

end CalendarYearIndexTivSuite
