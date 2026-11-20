package vecxt_re

/** A calendar year-based wrapper around IndexPerPeriod for on-leveling historical data.
  *
  * This class maps calendar years to index factors, allowing on-leveling of datasets where data points are labeled with
  * their calendar year.
  *
  * The factor labelled with year `Y` is the change from year `Y - 1` to year `Y`. Hence a value from year `Y` is
  * on-levelled to the current year by multiplying the factors of years `Y + 1` to `currentYear` inclusive, and the
  * earliest year that can be fully on-levelled is `earliestYear - 1`.
  *
  * @param currentYear
  *   The current/reference year (period 0). Must equal `years.head`.
  * @param years
  *   Array of contiguous years in descending order (most recent first), i.e. `years(i) == currentYear - i`. Use the
  *   companion `apply(years, indices)` to construct from years in any order.
  * @param indices
  *   Array of index factors corresponding to each year. All factors must be strictly positive.
  */
case class CalendarYearIndex(currentYear: Int, years: Array[Int], indices: Array[Double]):
  require(years.length == indices.length, "years and indices must have the same length")
  require(years.length > 0, "must provide at least one year")
  require(
    years.indices.forall(i => years(i) == currentYear - i),
    s"years must be contiguous, descending and start at currentYear ($currentYear), got ${years.mkString("[", ", ", "]")}"
  )
  require(indices.forall(_ > 0.0), s"all index factors must be > 0, got ${indices.mkString("[", ", ", "]")}")

  private val underlying: IndexPerPeriod = IndexPerPeriod(indices)

  /** Number of years covered by this index */
  inline def numYears: Int = years.length

  /** The earliest year covered */
  inline def earliestYear: Int = years.last

  /** The latest year covered (always equals currentYear) */
  inline def latestYear: Int = years.head

  /** The earliest data year which can be on-levelled using only the factors in this index. */
  inline def earliestOnLevelYear: Int = earliestYear - 1

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
    if year > currentYear || year < earliestYear then throw new NoSuchElementException(s"Year $year not in index")
    end if
    indices(currentYear - year)
  end indexAt

  /** Calculate the cumulative on-leveling factor from a historical year to the current year.
    *
    * Years after `currentYear` return 1.0.
    *
    * @param fromYear
    *   The historical year
    * @return
    *   The cumulative factor to on-level from that year to current
    * @throws IllegalArgumentException
    *   if `fromYear` is earlier than [[earliestOnLevelYear]] - the index does not cover it. Use the overload with a
    *   `fallbackFactor` to extrapolate.
    */
  def cumulativeToCurrentFrom(fromYear: Int): Double =
    require(
      fromYear >= earliestOnLevelYear,
      s"Year $fromYear is before the earliest year this index can on-level ($earliestOnLevelYear). Supply a fallbackFactor to extrapolate."
    )
    underlying.cumulativeToCurrentFrom(currentYear - fromYear)
  end cumulativeToCurrentFrom

  /** Calculate the cumulative on-leveling factor from a historical year to the current year, extrapolating with a
    * constant `fallbackFactor` per year for any years before those covered by the index.
    *
    * @param fromYear
    *   The historical year
    * @param fallbackFactor
    *   The factor applied for each year earlier than [[earliestOnLevelYear]] (e.g. 1.05 for 5% per year)
    */
  def cumulativeToCurrentFrom(fromYear: Int, fallbackFactor: Double): Double =
    require(fallbackFactor > 0.0, s"fallbackFactor must be > 0, got $fallbackFactor")
    if fromYear >= earliestOnLevelYear then cumulativeToCurrentFrom(fromYear)
    else
      val extraYears = earliestOnLevelYear - fromYear
      cumulativeToCurrentFrom(earliestOnLevelYear) * math.pow(fallbackFactor, extraYears)
    end if
  end cumulativeToCurrentFrom

  /** On-level a single value from `dataYear` to the current year.
    *
    * @param value
    *   The historical value, in `dataYear` terms
    * @param dataYear
    *   The calendar year of the value
    * @return
    *   The value in current-year terms
    * @throws IllegalArgumentException
    *   if `dataYear` is earlier than [[earliestOnLevelYear]]
    */
  def onLevel(value: Double, dataYear: Int): Double =
    value * cumulativeToCurrentFrom(dataYear)

  /** On-level a single value from `dataYear` to the current year, extrapolating with `fallbackFactor` per year if
    * `dataYear` is earlier than the index covers.
    */
  def onLevel(value: Double, dataYear: Int, fallbackFactor: Double): Double =
    value * cumulativeToCurrentFrom(dataYear, fallbackFactor)

  /** Apply on-leveling to an array of values, given their corresponding years.
    *
    * @param values
    *   The historical values to on-level
    * @param dataYears
    *   The calendar year for each value (same length as values)
    * @return
    *   Array of on-leveled values
    * @throws IllegalArgumentException
    *   if any data year is earlier than [[earliestOnLevelYear]]
    */
  def onLevel(values: Array[Double], dataYears: Array[Int]): Array[Double] =
    onLevelWith(values, dataYears)(cumulativeToCurrentFrom)

  /** Apply on-leveling to an array of values, extrapolating with `fallbackFactor` per year for data years earlier than
    * the index covers.
    */
  def onLevel(values: Array[Double], dataYears: Array[Int], fallbackFactor: Double): Array[Double] =
    onLevelWith(values, dataYears)(y => cumulativeToCurrentFrom(y, fallbackFactor))

  private inline def onLevelWith(values: Array[Double], dataYears: Array[Int])(
      inline factor: Int => Double
  ): Array[Double] =
    require(values.length == dataYears.length, "values and dataYears must have the same length")
    val result = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      result(i) = values(i) * factor(dataYears(i))
      i += 1
    end while
    result
  end onLevelWith

  /** The reporting threshold in current-year terms, for a threshold that was set in `thresholdYear`.
    *
    * @param reportThreshold
    *   The threshold, in `thresholdYear` money
    * @param thresholdYear
    *   The year the threshold was set
    */
  def suggestedNewThreshold(reportThreshold: Double, thresholdYear: Int): Double =
    reportThreshold * cumulativeToCurrentFrom(thresholdYear)

  override def equals(that: Any): Boolean = that match
    case o: CalendarYearIndex =>
      currentYear == o.currentYear &&
      java.util.Arrays.equals(years, o.years) &&
      java.util.Arrays.equals(indices, o.indices)
    case _ => false

  override def hashCode: Int =
    31 * (31 * currentYear.hashCode + java.util.Arrays.hashCode(years)) + java.util.Arrays.hashCode(indices)

  override def toString: String =
    val pairs = years.zip(indices).map((y, f) => s"$y -> $f").mkString(", ")
    s"CalendarYearIndex(currentYear=$currentYear, $pairs)"
  end toString

end CalendarYearIndex

object CalendarYearIndex:

  /** Create a CalendarYearIndex from arrays of years and their corresponding indices. Years may be provided in any
    * order, but must be contiguous (no gaps) and unique. The current year is set to the most recent year.
    *
    * @param years
    *   Array of calendar years
    * @param indices
    *   Array of index factors for each year
    * @return
    *   CalendarYearIndex with the current year set to the most recent year
    */
  def apply(years: Array[Int], indices: Array[Double]): CalendarYearIndex =
    require(years.length > 0, "must provide at least one year")
    require(years.length == indices.length, "years and indices must have the same length")
    require(years.distinct.length == years.length, s"years must be unique, got ${years.mkString("[", ", ", "]")}")
    val order = years.indices.sortBy(i => -years(i)).toArray
    val sortedYears = order.map(years(_))
    val sortedIndices = order.map(indices(_))
    CalendarYearIndex(sortedYears.head, sortedYears, sortedIndices)
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

  /** Create a CalendarYearIndex from arrays of years and rate changes (as percentages). Years may be provided in any
    * order, but must be contiguous and unique.
    *
    * @param years
    *   Array of calendar years
    * @param rateChanges
    *   Array of percentage changes for each year (e.g., 5.0 for 5%)
    * @return
    *   CalendarYearIndex with rate changes converted to factors
    */
  def fromRateChanges(years: Array[Int], rateChanges: Array[Double]): CalendarYearIndex =
    require(years.length == rateChanges.length, "years and rateChanges must have the same length")
    CalendarYearIndex(years, rateChanges.map(r => 1.0 + r / 100.0))
  end fromRateChanges

end CalendarYearIndex
