package vecxt_re

import vecxt.all.*

import java.time.LocalDate
import java.time.temporal.ChronoUnit
import java.time.Month

case class Scenarr(
    iterations: Array[Int],
    days: Array[Int],
    amounts: Array[Double],
    numberIterations: Int = 0,
    threshold: Double = 0d,
    day1: LocalDate = LocalDate.of(2019, 1, 1),
    name: String = "",
    id: Long = scala.util.Random.nextLong(),
    isSorted: Boolean = false
):
  assert(iterations.length == days.length && days.length == amounts.length)

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

  lazy val itrDayAmount: (itr: Array[Int], days: Array[Int], amounts: Array[Double]) =
    (itr = iterations, days = days, amounts = amounts)

  lazy val period: (firstLoss: LocalDate, lastLoss: LocalDate) =
    (day1.plusDays((days.minSIMD - 1).toLong), day1.plusDays((days.maxSIMD - 1).toLong))

end Scenarr

object Scenarr:
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
        scenario.numberIterations,
        scenario.threshold,
        scenario.day1,
        scenario.name,
        scenario.id,
        isSorted = true
      )
    end sorted

    inline def scaleAmntBy(scale: Double): Scenarr =
      scenario.copy(amounts = scenario.amounts * scale, threshold = scenario.threshold * scale)

    inline def iteration(num: Int) =
      assert(num > 0 && num <= scenario.numberIterations)
      val idx = scenario.iterations =:= num
      import vecxt.BoundsCheck.DoBoundsCheck.yes
      Scenarr(
        scenario.iterations.mask(idx),
        scenario.days.mask(idx),
        scenario.amounts.mask(idx),
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
        scenario.numberIterations,
        newThresh,
        scenario.day1,
        scenario.name
      )
    end applyThreshold
  end extension
end Scenarr
