package vecxt_re

import io.github.quafadas.plots.SetupVega.{*, given}
import viz.macros.VegaPlot
import io.circe.syntax.*

/** A calendar year-based wrapper around IndexPerPeriod for on-leveling historical data.
  *
  * This class maps calendar years to index factors, allowing on-leveling of datasets where data points are labeled with
  * their calendar year.
  *
  * @param currentYear
  *   The current/reference year (period 0)
  * @param years
  *   Array of years in descending order (most recent first)
  * @param indices
  *   Array of index factors corresponding to each year
  */
case class CalendarYearIndex(currentYear: Int, years: Array[Int], indices: Array[Double]):
  require(years.length == indices.length, "years and indices must have the same length")
  require(years.length > 0, "must provide at least one year")

  private val yearToIdx: Map[Int, Int] = years.zipWithIndex.toMap
  private val underlying: IndexPerPeriod = IndexPerPeriod(indices)

  /** Number of years covered by this index */
  inline def numYears: Int = years.length

  /** The earliest year covered */
  inline def earliestYear: Int = years.last

  /** The latest year covered (should equal currentYear if properly constructed) */
  inline def latestYear: Int = years.head

  /** Get the index factor for a specific year.
    *
    * @param year
    *   The calendar year
    * @return
    *   The index factor for that year
    * @throws NoSuchElementException
    *   if year is not in the index
    */
  def indexAt(year: Int): Double =
    val idx = yearToIdx.getOrElse(year, throw new NoSuchElementException(s"Year $year not in index"))
    indices(idx)
  end indexAt

  /** Calculate the cumulative on-leveling factor from a historical year to the current year.
    *
    * @param fromYear
    *   The historical year
    * @return
    *   The cumulative factor to on-level from that year to current
    */
  def cumulativeToCurrentFrom(fromYear: Int): Double =
    val periodsBack = currentYear - fromYear
    underlying.cumulativeToCurrentFrom(periodsBack)
  end cumulativeToCurrentFrom

  /** Apply on-leveling to an array of values, given their corresponding years.
    *
    * @param values
    *   The historical values to on-level
    * @param dataYears
    *   The calendar year for each value (same length as values)
    * @return
    *   Array of on-leveled values
    */
  def onLevel(values: Array[Double], dataYears: Array[Int]): Array[Double] =
    require(values.length == dataYears.length, "values and dataYears must have the same length")
    val result = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      result(i) = values(i) * cumulativeToCurrentFrom(dataYears(i))
      i += 1
    end while
    result
  end onLevel

  def suggestedNewThreshold(reportThreshold: Double): Double =
    val periodBack = currentYear - latestYear
    val factor = underlying.cumulativeToCurrentFrom(periodBack)
    reportThreshold * factor
  end suggestedNewThreshold

end CalendarYearIndex

object CalendarYearIndex:

  extension (idx: CalendarYearIndex)
    def plotIndex(reportingThreshold: Double)(using viz.LowPriorityPlotTarget) =
      val linePlot2 = VegaPlot.fromResource("index.vl.json")
      val cumulative = idx.onLevel(Array.fill(idx.years.length)(1.0), idx.years)
      val factors = idx.years.zip(idx.indices).zip(cumulative).map { case ((year, index), cumulative) =>
        (
          year = year,
          index = index,
          missing = 1 / cumulative,
          threshold = idx.suggestedNewThreshold(reportingThreshold)
        )
      }
      linePlot2.plot(
        _.data.values := factors.asJson
      )
  end extension

  /** Create a CalendarYearIndex from arrays of years and their corresponding indices. Years should be provided in
    * descending order (most recent first).
    *
    * @param years
    *   Array of calendar years in descending order
    * @param indices
    *   Array of index factors for each year
    * @return
    *   CalendarYearIndex with the current year set to the first (most recent) year
    */
  def apply(years: Array[Int], indices: Array[Double]): CalendarYearIndex =
    require(years.length > 0, "must provide at least one year")
    CalendarYearIndex(years.head, years, indices)
  end apply

  /** Create a CalendarYearIndex from a range of years with a constant rate change.
    *
    * @param fromYear
    *   The earliest year (inclusive)
    * @param toYear
    *   The current/latest year (inclusive)
    * @param factor
    *   The constant factor for each year (e.g., 1.05 for 5% per year)
    * @return
    *   CalendarYearIndex spanning the specified years
    */
  def constant(fromYear: Int, toYear: Int, factor: Double): CalendarYearIndex =
    require(toYear >= fromYear, "toYear must be >= fromYear")
    val numYears = toYear - fromYear + 1
    val years = Array.tabulate(numYears)(i => toYear - i)
    val indices = Array.fill(numYears)(factor)
    CalendarYearIndex(toYear, years, indices)
  end constant

  /** Create a CalendarYearIndex from arrays of years and rate changes (as percentages). Years should be provided in
    * descending order (most recent first).
    *
    * @param years
    *   Array of calendar years in descending order
    * @param rateChanges
    *   Array of percentage changes for each year (e.g., 5.0 for 5%)
    * @return
    *   CalendarYearIndex with rate changes converted to factors
    */
  def fromRateChanges(years: Array[Int], rateChanges: Array[Double]): CalendarYearIndex =
    require(years.length == rateChanges.length, "years and rateChanges must have the same length")
    val factors = new Array[Double](rateChanges.length)
    var i = 0
    while i < rateChanges.length do
      factors(i) = 1.0 + rateChanges(i) / 100.0
      i += 1
    end while
    CalendarYearIndex(years, factors)
  end fromRateChanges

end CalendarYearIndex
