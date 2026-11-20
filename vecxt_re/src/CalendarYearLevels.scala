package vecxt_re

/** A calendar year index of levels, for on-leveling historical data.
  *
  * Holds an index _level_ per calendar year (e.g. total insured value, a price index, or any series whose ratio between
  * two years gives the change between them). A value from year `Y` is on-levelled to the current year by multiplying by
  * `level(currentYear) / level(Y)`.
  *
  * For year-on-year change factors (e.g. 1.05 for +5%) rather than levels, use [[CalendarYearRateChanges]].
  *
  * @param currentYear
  *   The current/reference year. Must equal `years.head`.
  * @param years
  *   Array of contiguous years in descending order (most recent first), i.e. `years(i) == currentYear - i`. Use the
  *   companion `apply(years, levels)` to construct from years in any order.
  * @param levels
  *   Array of index levels corresponding to each year. All levels must be strictly positive.
  */
case class CalendarYearLevels(currentYear: Int, years: Array[Int], levels: Array[Double]):
  require(years.length == levels.length, "years and levels must have the same length")
  require(years.length > 0, "must provide at least one year")
  require(
    years.indices.forall(i => years(i) == currentYear - i),
    s"years must be contiguous, descending and start at currentYear ($currentYear), got ${years.mkString("[", ", ", "]")}"
  )
  require(levels.forall(_ > 0.0), s"all index levels must be > 0, got ${levels.mkString("[", ", ", "]")}")

  /** Number of years covered by this index */
  inline def numYears: Int = years.length

  /** The earliest year covered */
  inline def earliestYear: Int = years.last

  /** The latest year covered (always equals currentYear) */
  inline def latestYear: Int = years.head

  /** Get the index level for a specific year.
    *
    * @param year
    *   The calendar year
    * @return
    *   The index level for that year
    * @throws NoSuchElementException
    *   if year is not in the index
    */
  def levelAt(year: Int): Double =
    if year > currentYear || year < earliestYear then throw new NoSuchElementException(s"Year $year not in index")
    end if
    levels(currentYear - year)
  end levelAt

  /** The factor which on-levels a value from `fromYear` to the current year: `level(currentYear) / level(fromYear)`.
    *
    * Years after `currentYear` return 1.0.
    *
    * @param fromYear
    *   The historical year
    * @throws IllegalArgumentException
    *   if `fromYear` is earlier than [[earliestYear]]. Use the overload with a `fallbackFactor` to extrapolate.
    */
  def onLevelFactor(fromYear: Int): Double =
    require(
      fromYear >= earliestYear,
      s"Year $fromYear is before the earliest year in the index ($earliestYear). Supply a fallbackFactor to extrapolate."
    )
    if fromYear >= currentYear then 1.0 else levels(0) / levels(currentYear - fromYear)
    end if
  end onLevelFactor

  /** The factor which on-levels a value from `fromYear` to the current year, extrapolating the index backwards with a
    * constant `fallbackFactor` growth per year for any years before [[earliestYear]].
    *
    * @param fromYear
    *   The historical year
    * @param fallbackFactor
    *   The assumed growth per year before [[earliestYear]] (e.g. 1.05 for 5% per year)
    */
  def onLevelFactor(fromYear: Int, fallbackFactor: Double): Double =
    require(fallbackFactor > 0.0, s"fallbackFactor must be > 0, got $fallbackFactor")
    if fromYear >= earliestYear then onLevelFactor(fromYear)
    else onLevelFactor(earliestYear) * math.pow(fallbackFactor, earliestYear - fromYear)
    end if
  end onLevelFactor

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
    *   if `fromYear` is earlier than [[earliestYear]]
    */
  def onLevel(value: Double, fromYear: Int): Double =
    value * onLevelFactor(fromYear)

  /** On-level a single value from `fromYear` to the current year, extrapolating with `fallbackFactor` per year if
    * `fromYear` is earlier than the index covers.
    * {{{
    * idx.onLevel(value = 1.0, fromYear = 2015, fallbackFactor = 1.09)
    * }}}
    */
  def onLevel(value: Double, fromYear: Int, fallbackFactor: Double): Double =
    value * onLevelFactor(fromYear, fallbackFactor)

  /** Apply on-leveling to an array of values, given their corresponding years.
    *
    * @param values
    *   The historical values to on-level
    * @param fromYears
    *   The calendar year for each value (same length as values)
    * @return
    *   Array of on-leveled values
    * @throws IllegalArgumentException
    *   if any year in `fromYears` is earlier than [[earliestYear]]
    */
  def onLevel(values: Array[Double], fromYears: Array[Int]): Array[Double] =
    onLevelWith(values, fromYears)(onLevelFactor)

  /** Apply on-leveling to an array of values, extrapolating with `fallbackFactor` per year for years earlier than the
    * index covers.
    */
  def onLevel(values: Array[Double], fromYears: Array[Int], fallbackFactor: Double): Array[Double] =
    onLevelWith(values, fromYears)(y => onLevelFactor(y, fallbackFactor))

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
    reportThreshold * onLevelFactor(thresholdYear)

  override def equals(that: Any): Boolean = that match
    case o: CalendarYearLevels =>
      currentYear == o.currentYear &&
      java.util.Arrays.equals(years, o.years) &&
      java.util.Arrays.equals(levels, o.levels)
    case _ => false

  override def hashCode: Int =
    31 * (31 * currentYear.hashCode + java.util.Arrays.hashCode(years)) + java.util.Arrays.hashCode(levels)

  override def toString: String =
    val pairs = years.zip(levels).map((y, f) => s"$y -> $f").mkString(", ")
    s"CalendarYearLevels(currentYear=$currentYear, $pairs)"
  end toString

end CalendarYearLevels

object CalendarYearLevels:

  /** Create a CalendarYearLevels from arrays of years and their corresponding index levels. Years may be provided in
    * any order, but must be contiguous (no gaps) and unique. The current year is set to the most recent year.
    *
    * @param years
    *   Array of calendar years
    * @param levels
    *   Array of index levels for each year
    * @return
    *   CalendarYearLevels with the current year set to the most recent year
    */
  def apply(years: Array[Int], levels: Array[Double]): CalendarYearLevels =
    require(years.length > 0, "must provide at least one year")
    require(years.length == levels.length, "years and levels must have the same length")
    require(years.distinct.length == years.length, s"years must be unique, got ${years.mkString("[", ", ", "]")}")
    val order = years.indices.sortBy(i => -years(i)).toArray
    val sortedYears = order.map(years(_))
    val sortedLevels = order.map(levels(_))
    CalendarYearLevels(sortedYears.head, sortedYears, sortedLevels)
  end apply

end CalendarYearLevels
