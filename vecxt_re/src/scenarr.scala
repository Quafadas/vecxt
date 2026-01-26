package vecxt_re

import vecxt.all.*
import cats.kernel.Monoid

import java.time.LocalDate
import java.time.temporal.ChronoUnit
import java.time.Month
import scala.collection.mutable

case class Scenarr(
    iterations: Array[Int],
    days: Array[Int],
    amounts: Array[Double],
    ids: Array[Long],
    numberIterations: Int,
    threshold: Double = 0d,
    day1: LocalDate = LocalDate.of(2019, 1, 1),
    name: String = "",
    id: Long = scala.util.Random.nextLong(),
    isSorted: Boolean = false
):
  assert(
    iterations.length == days.length && days.length == amounts.length && amounts.length == ids.length,
    s"Array lengths must match: iterations=${iterations.length}, days=${days.length}, amounts=${amounts.length}, ids=${ids.length}"
  )

  lazy val freq: Array[Int] =
    assert(isSorted, "Scenario must be sorted to compute frequency")
    groupCount(iterations, numberIterations)
  end freq

  lazy val meanFreq: Double =
    freq.mean

  lazy val agg: Array[Double] =
    assert(isSorted, "Scenario must be sorted to compute aggregate amounts")
    groupSum(iterations, amounts, numberIterations)
  end agg

  lazy val claimDates: Array[LocalDate] = (days - 1).map(d => ChronoUnit.DAYS.addTo(this.day1, d))

  lazy val monthYear: Array[(month: Month, year: Int)] = claimDates.map(d => (d.getMonth, d.getYear))

  /** Interpretation:
    *
    *   - Excess variance over Poisson, scaled by m^2: Var(X) = E[X] for Poisson, so (v - m) is the extra variance;
    *     dividing by m^2 scales it.
    *   - Method-of-moments estimate of 1/k for Negative Binomial: Var(X) = μ + μ^2 / k ⇒ (Var(X) - μ) / μ^2 = 1 / k.
    *     Thus, clusterCoeff estimates 1 / k. Smaller k (larger clusterCoeff) ⇒ more clustering/overdispersion.
    *   - Relation to Index of Dispersion (VMR = v / m): clusterCoeff = (v - m) / m^2 = (VMR - 1) / m. It is a
    *     mean-scaled excess dispersion; under NB, it targets 1 / k.
    */
  lazy val clusterCoeff: Double =
    val (m, v) = freq.meanAndVariance(VarianceMode.Sample)
    (v - m) / Math.pow(m, 2)
  end clusterCoeff

  /** Computes the variance-to-mean ratio (dispersion) based on the frequency data. This metric is calculated by
    * dividing the variance by the mean, using values from `freq.meanAndVariance`.
    *
    * 1 = poisson distributed > 1 => overdispersed... but careful with sample size.
    */
  lazy val varianceMeanRatio =
    val (m, v) = freq.meanAndVariance(VarianceMode.Sample)
    v / m
  end varianceMeanRatio

  lazy val hasOccurence: Boolean = amounts.nonEmpty

  lazy val numSeasons: Int = math.ceil(days.maxSIMD.toDouble / 365).toInt // doesnt deal so well with leap years.

  lazy val meanLoss: Double = amounts.sum / numberIterations

  lazy val itrDayAmount: Array[(itr: Int, day: Int, amnt: Double, id: Long)] =
    iterations.zip(days).zip(amounts).zip(ids).map { case (((i, d), a), id) => (itr = i, day = d, amnt = a, id = id) }

  lazy val period: (firstLoss: LocalDate, lastLoss: LocalDate) =
    (day1.plusDays((days.minSIMD - 1).toLong), day1.plusDays((days.maxSIMD - 1).toLong))

end Scenarr

object Scenarr:
  /** The empty Scenarr - identity element for the monoid.
    * Combining any scenario with empty returns the original scenario unchanged.
    */
  val empty: Scenarr = new Scenarr(
    Array.emptyIntArray,
    Array.emptyIntArray,
    Array.emptyDoubleArray,
    Array.emptyLongArray,
    numberIterations = 0,
    threshold = 0d,
    day1 = LocalDate.of(2019, 1, 1),
    name = "empty",
    id = 0L,
    isSorted = true
  )

  /** Combine two Scenarr instances following monoid laws.
    *
    * The combination semantics are:
    *   - Events with matching IDs have their amounts aggregated (with validation that iteration/day match)
    *   - Thresholds are summed, and claims below the new threshold are filtered out
    *   - Day1 is the earlier of the two; the later scenario's days are adjusted to align calendar dates
    *   - Number of iterations must match (unless one is empty)
    *   - Result is always sorted by (iteration, day)
    *
    * @throws IllegalArgumentException if events with same ID have different iteration/day
    * @throws IllegalArgumentException if numberIterations don't match (for non-empty scenarios)
    */
  def combine(s1: Scenarr, s2: Scenarr): Scenarr =
    // Handle empty cases - identity element
    if s1.amounts.isEmpty then return s2
    if s2.amounts.isEmpty then return s1

    // Check iteration count matches for non-empty scenarios
    require(
      s1.numberIterations == s2.numberIterations,
      s"Cannot combine scenarios with different iteration counts: ${s1.numberIterations} vs ${s2.numberIterations}"
    )

    // Determine new day1 (earlier of the two)
    val newDay1 = if s1.day1.isBefore(s2.day1) then s1.day1 else s2.day1

    // Calculate day offsets to align both scenarios to newDay1
    val dayOffset1 = ChronoUnit.DAYS.between(newDay1, s1.day1).toInt
    val dayOffset2 = ChronoUnit.DAYS.between(newDay1, s2.day1).toInt

    // Sum thresholds
    val newThreshold = s1.threshold + s2.threshold

    // Build a map: id -> (iteration, adjustedDay, totalAmount)
    // This aggregates amounts for events with the same ID
    val idMap = mutable.HashMap.empty[Long, (Int, Int, Double)]

    // Process s1 events
    var i = 0
    while i < s1.ids.length do
      val id = s1.ids(i)
      val iter = s1.iterations(i)
      val day = s1.days(i) + dayOffset1
      val amount = s1.amounts(i)

      idMap.get(id) match
        case None =>
          idMap(id) = (iter, day, amount)
        case Some((existingIter, existingDay, existingAmount)) =>
          require(
            existingIter == iter && existingDay == day,
            s"Event with ID $id has inconsistent iteration/day: ($existingIter, $existingDay) vs ($iter, $day)"
          )
          idMap(id) = (iter, day, existingAmount + amount)
      i += 1
    end while

    // Process s2 events
    i = 0
    while i < s2.ids.length do
      val id = s2.ids(i)
      val iter = s2.iterations(i)
      val day = s2.days(i) + dayOffset2
      val amount = s2.amounts(i)

      idMap.get(id) match
        case None =>
          idMap(id) = (iter, day, amount)
        case Some((existingIter, existingDay, existingAmount)) =>
          require(
            existingIter == iter && existingDay == day,
            s"Event with ID $id has inconsistent iteration/day: ($existingIter, $existingDay) vs ($iter, $day)"
          )
          idMap(id) = (iter, day, existingAmount + amount)
      i += 1
    end while

    // Convert to arrays, sorted by (iteration, day), filtering by threshold
    val filtered = idMap.iterator.filter(_._2._3 > newThreshold).toArray
    val sorted = filtered.sortBy { case (_, (iter, day, _)) => (iter, day) }

    val finalIds = sorted.map(_._1)
    val finalIterations = sorted.map(_._2._1)
    val finalDays = sorted.map(_._2._2)
    val finalAmounts = sorted.map(_._2._3)

    new Scenarr(
      finalIterations,
      finalDays,
      finalAmounts,
      finalIds,
      s1.numberIterations,
      newThreshold,
      newDay1,
      s"concat: [${s1.name} + ${s2.name}]",
      scala.util.Random.nextLong(),
      isSorted = true
    )
  end combine

  /** Infix operator for combining scenarios */
  extension (s1: Scenarr) def |+|(s2: Scenarr): Scenarr = combine(s1, s2)

  /** Cats Monoid instance for Scenarr.
    *
    * This instance requires that all combined scenarios have the same `numberIterations`.
    * The identity element is `Scenarr.empty` with `numberIterations = 0`.
    *
    * Important: This monoid is only valid for scenarios with matching `numberIterations`.
    * Combining scenarios with different iteration counts will throw an IllegalArgumentException.
    *
    * @param numIterations The fixed number of iterations for this monoid instance
    */
  def monoidForIterations(numIterations: Int): Monoid[Scenarr] = new Monoid[Scenarr]:
    def empty: Scenarr = new Scenarr(
      Array.emptyIntArray,
      Array.emptyIntArray,
      Array.emptyDoubleArray,
      Array.emptyLongArray,
      numberIterations = numIterations,
      threshold = 0d,
      day1 = LocalDate.of(2019, 1, 1),
      name = "empty",
      id = 0L,
      isSorted = true
    )
    def combine(x: Scenarr, y: Scenarr): Scenarr = Scenarr.combine(x, y)
  end monoidForIterations

  /** Default Monoid instance for Scenarr.
    * Uses the general `combine` which treats empty scenarios as identity.
    */
  given Monoid[Scenarr] with
    def empty: Scenarr = Scenarr.empty
    def combine(x: Scenarr, y: Scenarr): Scenarr = Scenarr.combine(x, y)
  end given

  /** Create a Scenarr with automatically generated random IDs for each event.
    * Use this factory when you don't need to specify event IDs explicitly.
    */
  def withGeneratedIds(
      iterations: Array[Int],
      days: Array[Int],
      amounts: Array[Double],
      numberIterations: Int,
      threshold: Double = 0d,
      day1: LocalDate = LocalDate.of(2019, 1, 1),
      name: String = "",
      id: Long = scala.util.Random.nextLong(),
      isSorted: Boolean = false
  ): Scenarr =
    val ids = Array.fill(iterations.length)(scala.util.Random.nextLong())
    new Scenarr(iterations, days, amounts, ids, numberIterations, threshold, day1, name, id, isSorted)
  end withGeneratedIds

  extension (scenario: Scenarr)
    inline def sorted: Scenarr =
      val indicies = scenario.iterations.zipWithIndex
        .zip(scenario.days)
        .map { case ((iter, idx), day) =>
          (index = idx, iter = iter, day = day)
        }
        .sortBy(r => (r.iter, r.day))
        .map(_.index)

      Scenarr(
        scenario.iterations.select(indicies),
        scenario.days.select(indicies),
        scenario.amounts.select(indicies),
        scenario.ids.select(indicies),
        scenario.numberIterations,
        scenario.threshold,
        scenario.day1,
        scenario.name,
        scenario.id,
        isSorted = true
      )
    end sorted

    inline def takeFirstNIterations(i: Int) =
      assert(i > 0 && i <= scenario.numberIterations)
      val idx = scenario.iterations <= i
      import vecxt.BoundsCheck.DoBoundsCheck.yes
      Scenarr(
        scenario.iterations.mask(idx),
        scenario.days.mask(idx),
        scenario.amounts.mask(idx),
        scenario.ids.mask(idx),
        i,
        scenario.threshold,
        scenario.day1,
        scenario.name,
        scenario.id,
        isSorted = scenario.isSorted
      )
    end takeFirstNIterations

    inline def scaleAmntBy(scale: Double): Scenarr =
      scenario.copy(amounts = scenario.amounts * scale, ids = scenario.ids, threshold = scenario.threshold * scale)

    inline def iteration(num: Int) =
      assert(num > 0 && num <= scenario.numberIterations)
      val idx = scenario.iterations =:= num
      import vecxt.BoundsCheck.DoBoundsCheck.yes
      Scenarr(
        scenario.iterations.mask(idx),
        scenario.days.mask(idx),
        scenario.amounts.mask(idx),
        scenario.ids.mask(idx),
        scenario.numberIterations,
        scenario.threshold,
        scenario.day1,
        scenario.name,
        scenario.id,
        isSorted = scenario.isSorted
      )
    end iteration

    //   def shiftDay1To(date: LocalDate): Scenarr =
    //     scenario.period.firstLoss.plusYears(1).minusDays(1)
    // //    val ndays = ChronoUnit.DAYS.between(  period._1, seasonEnd) + 1 Let sjust ssume this is 365 ... there is a theoretical problem with air assuming 365 days. Leap years anyone?
    //     val betweenStartDates = ChronoUnit.DAYS.between(scenario.day1, date).toInt
    //     val newEvents =
    //       scenario.eventsSorted.map(x =>
    //         Event(x.eventId, x.iteration, Math.floorMod(x.day - betweenStartDates - 1, 365) + 1, x.loss)
    //       )
    //     Scenario(newEvents, scenario.numberIterations, scenario.threshold, date, scenario.name)
    //   end shiftDay1To

    //   inline def removeClaimsAfter(date: LocalDate): Scenarr =
    //     val remaining = scenario.claimDates.zip(scenario.eventsSorted).filter(_._1.compareTo(date) <= 0)
    //     Scenario(remaining.map(_._2), scenario.numberIterations, scenario.threshold, scenario.day1, scenario.name)
    //   end removeClaimsAfter

    //   inline def removeClaimsBefore(date: LocalDate): Scenarr =
    //     val remaining = scenario.claimDates.zip(scenario.eventsSorted).filter(_._1.compareTo(date) >= 0)
    //     Scenario(remaining.map(_._2), scenario.numberIterations, scenario.threshold, scenario.day1, scenario.name)
    //   end removeClaimsBefore

    inline def applyThreshold(newThresh: Double): Scenarr =
      if !(newThresh > scenario.threshold) then
        throw new Exception(
          "Threshold may only be increased. Attempt to change it from " + scenario.threshold + " to " + newThresh + " is illegal"
        )
      end if
      val idx = scenario.amounts > newThresh
      Scenarr(
        scenario.iterations.mask(idx)(using false),
        scenario.days.mask(idx)(using false),
        scenario.amounts.mask(idx)(using false),
        scenario.ids.mask(idx)(using false),
        scenario.numberIterations,
        newThresh,
        scenario.day1,
        scenario.name
      )
    end applyThreshold
  end extension
end Scenarr
