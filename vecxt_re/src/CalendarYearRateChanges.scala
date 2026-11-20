package vecxt_re

/** Year-on-year rate changes by calendar year, for on-leveling historical data.
  *
  * Each entry is the _change factor_ for a year (e.g. 1.05 for +5%), not an index level. For an index level series
  * (TIV, CPI, ...) use [[CalendarYearLevels]] instead.
  *
  * The factor labelled with year `Y` is the change from year `Y - 1` to year `Y`. Hence a value from year `Y` is
  * on-levelled to the current year by multiplying the factors of years `Y + 1` to `currentYear` inclusive, and the
  * earliest year that can be fully on-levelled is `earliestYear - 1`.
  *
  * @param currentYear
  *   The current/reference year (period 0). Must equal `years.head`.
  * @param years
  *   Array of contiguous years in descending order (most recent first), i.e. `years(i) == currentYear - i`. Use the
  *   companion `apply(years, factors)` to construct from years in any order.
  * @param factors
  *   Array of change factors corresponding to each year. All factors must be strictly positive.
  *
  * The constructor is private: the companion `apply` methods defensively copy the input arrays, so mutating the arrays
  * passed in cannot bypass validation or desynchronise the cached cumulative factors.
  */
case class CalendarYearRateChanges private (currentYear: Int, years: Array[Int], factors: Array[Double]):
  require(years.length == factors.length, "years and factors must have the same length")
  require(years.length > 0, "must provide at least one year")
  require(
    years.indices.forall(i => years(i) == currentYear - i),
    s"years must be contiguous, descending and start at currentYear ($currentYear), got ${years.mkString("[", ", ", "]")}"
  )
  require(factors.forall(_ > 0.0), s"all change factors must be > 0, got ${factors.mkString("[", ", ", "]")}")

  private val underlying: RateChangesPerPeriod = RateChangesPerPeriod(factors)

  /** Number of years covered by this index */
  inline def numYears: Int = years.length

  /** The earliest year covered */
  inline def earliestYear: Int = years.last

  /** The latest year covered (always equals currentYear) */
  inline def latestYear: Int = years.head

  /** The earliest data year which can be on-levelled using only the factors in this index. */
  inline def earliestOnLevelYear: Int = earliestYear - 1

  /** Get the change factor for a specific year.
    *
    * @param year
    *   The calendar year
    * @return
    *   The change factor for that year
    * @throws NoSuchElementException
    *   if year is not in the index
    */
  def factorAt(year: Int): Double =
    if year > currentYear || year < earliestYear then throw new NoSuchElementException(s"Year $year not in index")
    end if
    factors(currentYear - year)
  end factorAt

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

  /** On-level a single value from `fromYear` to the current year.
    *
    * Prefer named arguments - an `Int` value silently widens to `Double`, so swapping the arguments compiles:
    * {{{
    * idx.onLevel(value = 1.0, fromYear = 2023)
    * }}}
    *
    * @param value
    *   The historical value, in `fromYear` terms
    * @param fromYear
    *   The calendar year of the value
    * @return
    *   The value in current-year terms
    * @throws IllegalArgumentException
    *   if `fromYear` is earlier than [[earliestOnLevelYear]]
    */
  def onLevel(value: Double, fromYear: Int): Double =
    value * cumulativeToCurrentFrom(fromYear)

  /** On-level a single value from `fromYear` to the current year, extrapolating with `fallbackFactor` per year if
    * `fromYear` is earlier than the index covers.
    * {{{
    * idx.onLevel(value = 1.0, fromYear = 2015, fallbackFactor = 1.09)
    * }}}
    */
  def onLevel(value: Double, fromYear: Int, fallbackFactor: Double): Double =
    value * cumulativeToCurrentFrom(fromYear, fallbackFactor)

  /** Apply on-leveling to an array of values, given their corresponding years.
    *
    * @param values
    *   The historical values to on-level
    * @param fromYears
    *   The calendar year for each value (same length as values)
    * @return
    *   Array of on-leveled values
    * @throws IllegalArgumentException
    *   if any year in `fromYears` is earlier than [[earliestOnLevelYear]]
    */
  def onLevel(values: Array[Double], fromYears: Array[Int]): Array[Double] =
    onLevelWith(values, fromYears)(cumulativeToCurrentFrom)

  /** Apply on-leveling to an array of values, extrapolating with `fallbackFactor` per year for data years earlier than
    * the index covers.
    */
  def onLevel(values: Array[Double], fromYears: Array[Int], fallbackFactor: Double): Array[Double] =
    onLevelWith(values, fromYears)(y => cumulativeToCurrentFrom(y, fallbackFactor))

  private inline def onLevelWith(values: Array[Double], fromYears: Array[Int])(
      inline factor: Int => Double
  ): Array[Double] =
    require(values.length == fromYears.length, "values and fromYears must have the same length")
    val result = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      result(i) = values(i) * factor(fromYears(i))
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
    case o: CalendarYearRateChanges =>
      currentYear == o.currentYear &&
      java.util.Arrays.equals(years, o.years) &&
      java.util.Arrays.equals(factors, o.factors)
    case _ => false

  override def hashCode: Int =
    31 * (31 * currentYear.hashCode + java.util.Arrays.hashCode(years)) + java.util.Arrays.hashCode(factors)

  override def toString: String =
    val pairs = years.zip(factors).map((y, f) => s"$y -> $f").mkString(", ")
    s"CalendarYearRateChanges(currentYear=$currentYear, $pairs)"
  end toString

end CalendarYearRateChanges

object CalendarYearRateChanges:

  /** Create a CalendarYearRateChanges with years already in contiguous descending order starting at `currentYear`.
    * The input arrays are copied.
    */
  def apply(currentYear: Int, years: Array[Int], factors: Array[Double]): CalendarYearRateChanges =
    new CalendarYearRateChanges(currentYear, years.clone(), factors.clone())

  /** Create a CalendarYearRateChanges from arrays of years and their corresponding change factors. Years may be
    * provided in any order, but must be contiguous (no gaps) and unique. The current year is set to the most recent
    * year.
    *
    * @param years
    *   Array of calendar years
    * @param factors
    *   Array of change factors for each year
    * @return
    *   CalendarYearRateChanges with the current year set to the most recent year
    */
  def apply(years: Array[Int], factors: Array[Double]): CalendarYearRateChanges =
    require(years.length > 0, "must provide at least one year")
    require(years.length == factors.length, "years and factors must have the same length")
    require(years.distinct.length == years.length, s"years must be unique, got ${years.mkString("[", ", ", "]")}")
    val order = years.indices.sortBy(i => -years(i)).toArray
    val sortedYears = order.map(years(_))
    val sortedFactors = order.map(factors(_))
    // freshly allocated arrays - no need to copy again
    new CalendarYearRateChanges(sortedYears.head, sortedYears, sortedFactors)
  end apply

  /** Create a CalendarYearRateChanges from a range of years with a constant rate change.
    *
    * @param fromYear
    *   The earliest year (inclusive)
    * @param toYear
    *   The current/latest year (inclusive)
    * @param factor
    *   The constant factor for each year (e.g., 1.05 for 5% per year)
    * @return
    *   CalendarYearRateChanges spanning the specified years
    */
  def constant(fromYear: Int, toYear: Int, factor: Double): CalendarYearRateChanges =
    require(toYear >= fromYear, "toYear must be >= fromYear")
    val numYears = toYear - fromYear + 1
    val years = Array.tabulate(numYears)(i => toYear - i)
    val factors = Array.fill(numYears)(factor)
    new CalendarYearRateChanges(toYear, years, factors)
  end constant

  /** Create a CalendarYearRateChanges from arrays of years and rate changes (as percentages). Years may be provided in
    * any order, but must be contiguous and unique.
    *
    * @param years
    *   Array of calendar years
    * @param rateChanges
    *   Array of percentage changes for each year (e.g., 5.0 for 5%)
    * @return
    *   CalendarYearRateChanges with rate changes converted to factors
    */
  def fromRateChanges(years: Array[Int], rateChanges: Array[Double]): CalendarYearRateChanges =
    require(years.length == rateChanges.length, "years and rateChanges must have the same length")
    CalendarYearRateChanges(years, rateChanges.map(r => 1.0 + r / 100.0))
  end fromRateChanges

end CalendarYearRateChanges
