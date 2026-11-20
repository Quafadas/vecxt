package vecxt_re

import java.time.LocalDate
import java.time.temporal.ChronoUnit

import vecxt.all.*

case class Event(eventId: Long = scala.util.Random.nextLong(), iteration: Int = 0, day: Int = 0, loss: Double = 0):
  def multiplyBy(scale: Double): Event = this.copy(loss = loss * scale)
end Event

object Event:
  inline def random(maxAmount: Double = 20, maxIter: Int = 10) =
    Event(iteration = scala.util.Random.nextInt(maxIter), loss = scala.util.Random.nextDouble() * maxAmount)
  inline def apply(iter: Int, amount: Double): Event = Event(
    iteration = iter,
    loss = amount
  )
end Event

// case class IterationFrequency(itr: Int, freq: Int)

// case class IterationAmount(itr: Int, amnt: Double)

case class Scenario(
    events: IndexedSeq[Event] = Vector(),
    numberIterations: Int = 0,
    threshold: Double = 0d,
    day1: LocalDate = LocalDate.of(2019, 1, 1),
    name: String = "",
    id: Long = scala.util.Random.nextLong()
):

  lazy val eventsSorted: Array[Event] = Array.from(events.sortBy(event => (event.iteration, event.day)))

  lazy val freq: Array[Int] = groupCount(iterations, numberIterations)

  lazy val meanFreq: Double = freq.mean

  lazy val agg: Array[Double] = groupSum(iterations, amounts, numberIterations)

  lazy val claimDates: Array[LocalDate] = eventsSorted.map(d => ChronoUnit.DAYS.addTo(this.day1, d.day))

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
    val stats = freq.meanAndVariance(VarianceMode.Sample)
    (stats.variance - stats.mean) / Math.pow(stats.mean, 2)
  end clusterCoeff

  /** Computes the variance-to-mean ratio (dispersion) based on the frequency data. This metric is calculated by
    * dividing the variance by the mean, using values from `freq.meanAndVariance`.
    *
    * 1 = poisson distributed > 1 => overdispersed... but careful with sample size.
    */
  lazy val varianceMeanRatio =
    val stats = freq.meanAndVariance(VarianceMode.Sample)
    stats.variance / stats.mean
  end varianceMeanRatio

  lazy val hasOccurence: Boolean = events.nonEmpty

  lazy val numSeasons: Int = math.ceil(days.maxSIMD / 365).toInt // doesnt deal so well with leap years.

  lazy val meanLoss: Double = amounts.sum / numberIterations

  lazy val days: Array[Int] = eventsSorted.map(_.day)

  lazy val iterations: Array[Int] = eventsSorted.map(_.iteration)

  lazy val amounts: Array[Double] = eventsSorted.map(_.loss)

  lazy val itrDayAmount: (itr: Array[Int], days: Array[Int], amounts: Array[Double]) =
    (itr = iterations, days = days, amounts = amounts)

  lazy val period: (firstLoss: LocalDate, lastLoss: LocalDate) =
    (day1.plusDays((days.minSIMD - 1).toLong), day1.plusDays((days.maxSIMD - 1).toLong))

end Scenario

object Scenario:

  /** A scenario with no events, belonging to a simulation of `numberIterations` iterations starting at `day1`.
    *
    * With the default zero threshold it leaves any scenario unchanged under `combine`. A positive threshold states that
    * nothing above it happened, and raises the threshold of anything it is combined with.
    */
  def empty(numberIterations: Int, day1: LocalDate, threshold: Double = 0d): Scenario =
    Scenario(Vector.empty, numberIterations, threshold, day1, "empty", 0L)

  /** Combine two scenarios. Same as `combineAll(Seq(s1, s2))`. */
  def combine(s1: Scenario, s2: Scenario): Scenario = combineAll(Seq(s1, s2))

  /** Combine scenarios from the same simulation into one, with the same semantics as `Scenarr.combineAll`:
    *
    *   - Events with the same `eventId` are one event, and their losses are summed. An ID must sit at the same
    *     (iteration, day) wherever it appears.
    *   - Scenarios with no events don't constrain anything. All scenarios that have events must agree on
    *     `numberIterations` and `day1`.
    *   - Scenarios linked by shared event IDs form a group whose threshold is the sum of its members' thresholds; the
    *     result's threshold is the largest group threshold. For two scenarios: `t1 + t2` if they share an ID,
    *     `max(t1, t2)` otherwise.
    *   - Events at or below the result's threshold are dropped, once, after all losses have been summed.
    *
    * Combining with non-zero thresholds is not associative, so prefer this over folding `combine`.
    *
    * @throws IllegalArgumentException
    *   if `scenarios` is empty, if scenarios with events disagree on `numberIterations` or `day1`, or if an event ID
    *   appears at more than one (iteration, day)
    */
  def combineAll(scenarios: Seq[Scenario]): Scenario =
    fromScenarr(Scenarr.combineAll(scenarios.map(toScenarr)))

  extension (s1: Scenario) def |+|(s2: Scenario): Scenario = combine(s1, s2)
  end extension

  private def toScenarr(s: Scenario): Scenarr =
    val ev = s.eventsSorted
    new Scenarr(
      ev.map(_.iteration),
      ev.map(_.day),
      ev.map(_.loss),
      ev.map(_.eventId),
      s.numberIterations,
      s.threshold,
      s.day1,
      s.name,
      s.id,
      isSorted = true
    )
  end toScenarr

  private def fromScenarr(s: Scenarr): Scenario =
    val events = Vector.tabulate(s.ids.length)(i => Event(s.ids(i), s.iterations(i), s.days(i), s.amounts(i)))
    Scenario(events, s.numberIterations, s.threshold, s.day1, s.name, s.id)
  end fromScenarr

end Scenario

extension (scenario: Scenario)
  inline def scaleAmntBy(scale: Double): Scenario = Scenario(
    scenario.eventsSorted.map(_.multiplyBy(scale)),
    scenario.numberIterations,
    scenario.threshold * scale,
    scenario.day1,
    scenario.name
  )

  def shiftDay1To(date: LocalDate): Scenario =
    scenario.period.firstLoss.plusYears(1).minusDays(1)
//    val ndays = ChronoUnit.DAYS.between(  period._1, seasonEnd) + 1 Let sjust ssume this is 365 ... there is a theoretical problem with air assuming 365 days. Leap years anyone?
    val betweenStartDates = ChronoUnit.DAYS.between(scenario.day1, date).toInt
    val newEvents =
      scenario.eventsSorted.map(x =>
        Event(x.eventId, x.iteration, Math.floorMod(x.day - betweenStartDates - 1, 365) + 1, x.loss)
      )
    Scenario(newEvents, scenario.numberIterations, scenario.threshold, date, scenario.name)
  end shiftDay1To

  inline def removeClaimsAfter(date: LocalDate): Scenario =
    val remaining = scenario.claimDates.zip(scenario.eventsSorted).filter(_._1.compareTo(date) <= 0)
    Scenario(remaining.map(_._2), scenario.numberIterations, scenario.threshold, scenario.day1, scenario.name)
  end removeClaimsAfter

  inline def removeClaimsBefore(date: LocalDate): Scenario =
    val remaining = scenario.claimDates.zip(scenario.eventsSorted).filter(_._1.compareTo(date) >= 0)
    Scenario(remaining.map(_._2), scenario.numberIterations, scenario.threshold, scenario.day1, scenario.name)
  end removeClaimsBefore

  inline def applyThreshold(newThresh: Double): Scenario =
    if !(newThresh > scenario.threshold) then
      throw new Exception(
        "Threshold may only be increased. Attempt to change it from " + scenario.threshold + " to " + newThresh + " is illegal"
      )
    end if
    Scenario(
      scenario.eventsSorted.filter(_.loss > newThresh),
      scenario.numberIterations,
      newThresh,
      scenario.day1,
      scenario.name
    )
  end applyThreshold
end extension
