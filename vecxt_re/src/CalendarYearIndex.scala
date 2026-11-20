package vecxt_re

/** A calendar year index for on-leveling historical data.
  *
  * Holds an index _level_ per calendar year (e.g. total insured value, a price index, or any series whose ratio
  * between two years gives the change between them). A value from year `Y` is on-levelled to the current year by
  * multiplying by `level(currentYear) / level(Y)`.
  *
  * @param currentYear
  *   The current/reference year. Must equal `years.head`.
  * @param years
  *   Array of contiguous years in descending order (most recent first), i.e. `years(i) == currentYear - i`. Use the
  *   companion `apply(years, indices)` to construct from years in any order.
  * @param indices
  *   Array of index levels corresponding to each year. All levels must be strictly positive.
  */
case class CalendarYearIndex(currentYear: Int, years: Array[Int], indices: Array[Double]):
  require(years.length == indices.length, "years and indices must have the same length")
  require(years.length > 0, "must provide at least one year")
  require(
    years.indices.forall(i => years(i) == currentYear - i),
    s"years must be contiguous, descending and start at currentYear ($currentYear), got ${years.mkString("[", ", ", "]")}"
  )
  require(indices.forall(_ > 0.0), s"all index levels must be > 0, got ${indices.mkString("[", ", ", "]")}")

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
  def indexAt(year: Int): Double =
    if year > currentYear || year < earliestYear then throw new NoSuchElementException(s"Year $year not in index")
    end if
    indices(currentYear - year)
  end indexAt

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
    if fromYear >= currentYear then 1.0 else indices(0) / indices(currentYear - fromYear)
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

  /** Create a CalendarYearIndex from arrays of years and their corresponding index levels. Years may be provided in
    * any order, but must be contiguous (no gaps) and unique. The current year is set to the most recent year.
    *
    * @param years
    *   Array of calendar years
    * @param indices
    *   Array of index levels for each year
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

  /** Create a CalendarYearIndex which grows by a constant factor each year, with level 1.0 in `fromYear`.
    *
    * @param fromYear
    *   The earliest year (inclusive)
    * @param toYear
    *   The current/latest year (inclusive)
    * @param factor
    *   The growth per year (e.g., 1.05 for 5% per year)
    * @return
    *   CalendarYearIndex spanning the specified years
    */
  def constant(fromYear: Int, toYear: Int, factor: Double): CalendarYearIndex =
    require(toYear >= fromYear, "toYear must be >= fromYear")
    require(factor > 0.0, s"factor must be > 0, got $factor")
    val years = Array.tabulate(toYear - fromYear + 1)(i => toYear - i)
    CalendarYearIndex(toYear, years, years.map(y => math.pow(factor, y - fromYear)))
  end constant

  /** Create a CalendarYearIndex from year-on-year rate changes (as percentages), where the change labelled `Y` is the
    * change from year `Y - 1` to year `Y`. Years may be provided in any order, but must be contiguous and unique.
    *
    * The resulting index has level 1.0 in the year before the earliest rate change, so it covers one more year than
    * `years`.
    *
    * @param years
    *   Array of calendar years
    * @param rateChanges
    *   Array of percentage changes for each year (e.g., 5.0 for 5%)
    */
  def fromRateChanges(years: Array[Int], rateChanges: Array[Double]): CalendarYearIndex =
    require(years.length > 0, "must provide at least one year")
    require(years.length == rateChanges.length, "years and rateChanges must have the same length")
    require(rateChanges.forall(_ > -100.0), s"rate changes must be > -100%, got ${rateChanges.mkString("[", ", ", "]")}")
    val ascending = years.zip(rateChanges).sortBy(_._1)
    val base = ascending.head._1 - 1
    val levelYears = base +: ascending.map(_._1)
    val levels = ascending.scanLeft(1.0) { case (level, (_, r)) => level * (1.0 + r / 100.0) }
    CalendarYearIndex(levelYears, levels)
  end fromRateChanges

end CalendarYearIndex
