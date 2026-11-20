package vecxt_re

import java.time.LocalDate
import java.time.Month
import java.time.temporal.ChronoUnit

import vecxt.all.*

/** Columnar scenario: one entry per event across `iterations`, `days`, `amounts` and `ids`.
  *
  * `isSorted` is a promise from the constructor's caller that the columns are ordered by (iteration, day), and that
  * every iteration lies in 1..numberIterations. It is trusted, not verified: per-iteration statistics (`freq`, `agg`,
  * `oep`) feed it into unchecked single-pass kernels (see groupSums.scala), and passing `isSorted = true` for unsorted
  * data gives silently wrong results. If you're unsure, leave it false and call `.sorted`.
  */
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

  lazy val aep = agg.sorted(using Ordering[Double].reverse).zipWithIndex.map { case (amt, idx) =>
    (returnPeriod = numberIterations.toDouble / (idx + 1).toDouble, amount = amt)
  }

  lazy val oep =
    assert(isSorted, "Scenario must be sorted to compute occurrence exceedance")
    groupMax(iterations, amounts, numberIterations).sorted(using Ordering[Double].reverse).zipWithIndex.map {
      case (amt, idx) =>
        (returnPeriod = numberIterations.toDouble / (idx + 1).toDouble, amount = amt)
    }
  end oep

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

  lazy val hasOccurence: Boolean = amounts.nonEmpty

  lazy val numSeasons: Int = math.ceil(days.maxSIMD.toDouble / 365).toInt // doesnt deal so well with leap years.

  lazy val meanLoss: Double = amounts.sum / numberIterations

  lazy val itrDayAmount: Array[(itr: Int, day: Int, amnt: Double, id: Long)] =
    iterations.zip(days).zip(amounts).zip(ids).map { case (((i, d), a), id) => (itr = i, day = d, amnt = a, id = id) }

  lazy val period: (firstLoss: LocalDate, lastLoss: LocalDate) =
    (day1.plusDays((days.minSIMD - 1).toLong), day1.plusDays((days.maxSIMD - 1).toLong))

end Scenarr

object Scenarr:
  /** A scenario with no events, belonging to a simulation of `numberIterations` iterations starting at `day1`.
    *
    * With the default zero threshold it leaves any scenario unchanged under `combine`. A positive threshold states that
    * nothing above it happened, and raises the threshold of anything it is combined with (see `combineAll`).
    */
  def empty(numberIterations: Int, day1: LocalDate, threshold: Double = 0d): Scenarr = new Scenarr(
    Array.emptyIntArray,
    Array.emptyIntArray,
    Array.emptyDoubleArray,
    Array.emptyLongArray,
    numberIterations = numberIterations,
    threshold = threshold,
    day1 = day1,
    name = "empty",
    id = 0L,
    isSorted = true
  )

  /** Generate a small random Scenarr for experimentation.
    *
    * @param numClaims
    *   number of claim events to generate (default 15)
    * @param seed
    *   optional random seed for reproducibility
    * @return
    *   a small Scenarr with 10 iterations
    */
  def sample(numClaims: Int = 15, seed: Option[Long] = None): Scenarr =
    val rng = seed.fold(scala.util.Random())(s => scala.util.Random(s))
    val n = numClaims

    val iterations = Array.fill(n)(rng.nextInt(10) + 1)
    val days = Array.fill(n)(rng.nextInt(365) + 1)
    val amounts = Array.fill(n)(rng.nextDouble() * 1000.0 + 100.0) // 100-1100
    val ids = Array.fill(n)(rng.nextLong())

    new Scenarr(
      iterations = iterations,
      days = days,
      amounts = amounts,
      ids = ids,
      numberIterations = 10,
      threshold = 0.0,
      day1 = LocalDate.of(2019, 1, 1),
      name = "sample",
      id = rng.nextLong(),
      isSorted = false
    )
  end sample

  /** Combine two scenarios. Same as `combineAll(Seq(s1, s2))`, which documents the semantics.
    *
    * In particular: `t1 + t2` threshold if the two share an event ID, `max(t1, t2)` otherwise.
    */
  def combine(s1: Scenarr, s2: Scenarr): Scenarr = combineAll(Seq(s1, s2))

  /** Combine scenarios from the same simulation into one.
    *
    *   - Events with the same ID are one event, and their amounts are summed. An ID must sit at the same (iteration,
    *     day) wherever it appears.
    *   - Scenarios with no events don't constrain anything. All scenarios that have events must agree on
    *     `numberIterations` and `day1`; the result takes both from the first scenario with events, or from the first
    *     scenario if none has any.
    *   - Threshold: scenarios that share event IDs (directly, or through a chain of other scenarios) form a group whose
    *     threshold is the sum of its members' thresholds, because an event split across them can be missing a piece
    *     below each member's threshold. Groups don't interact, so the result's threshold is the largest group
    *     threshold. A scenario sharing no IDs, including one with no events, is a group of its own.
    *   - Events at or below the result's threshold are dropped, once, after all amounts have been summed.
    *   - The result is sorted by (iteration, day).
    *
    * Combining with non-zero thresholds is not associative, because each binary combine filters. Combining a whole
    * collection in one call applies the threshold once, so prefer this method over folding `combine`. With zero
    * thresholds the result does not depend on grouping, and `Scenarr.empty(n, day1)` leaves any scenario unchanged.
    *
    * @throws IllegalArgumentException
    *   if `scenarios` is empty, if scenarios with events disagree on `numberIterations` or `day1`, or if an event ID
    *   appears at more than one (iteration, day)
    */
  def combineAll(scenarios: Seq[Scenarr]): Scenarr =
    require(scenarios.nonEmpty, "combineAll needs at least one scenario; use Scenarr.empty to construct an empty one")

    val withEvents = scenarios.filter(_.amounts.nonEmpty).toIndexedSeq
    val ref = withEvents.headOption.getOrElse(scenarios.head)
    withEvents.foreach { s =>
      require(
        s.numberIterations == ref.numberIterations,
        s"Cannot combine scenarios with different iteration counts: ${ref.numberIterations} vs ${s.numberIterations}"
      )
      require(s.day1 == ref.day1, s"Cannot combine scenarios with different day1: ${ref.day1} vs ${s.day1}")
    }

    // Scenarios without events are threshold groups of their own.
    val emptyThreshold = scenarios.iterator.filter(_.amounts.isEmpty).map(_.threshold).maxOption

    if withEvents.isEmpty then return empty(ref.numberIterations, ref.day1, emptyThreshold.get)
    end if

    // One scenario with events, and no empty scenario raising its threshold: it is the answer.
    if withEvents.length == 1 && emptyThreshold.forall(_ <= withEvents.head.threshold) then
      val s = withEvents.head
      return if s.isSorted then s else s.sorted
    end if

    val k = withEvents.length
    val merged = mergeAll(withEvents.indices.map { src =>
      val s = withEvents(src)
      val ss = if s.isSorted then s else s.sorted
      Cols(ss.iterations, ss.days, ss.amounts, ss.ids, Array.fill(ss.ids.length)(src))
    })

    // Union-find over source scenarios: two scenarios are joined when they share an event ID.
    val parent = Array.tabulate(k)(identity)
    def find(x: Int): Int =
      var r = x
      while parent(r) != r do r = parent(r)
      end while
      parent(x) = r
      r
    end find

    // Sum amounts of the same ID within each (iteration, day) slot. Slots hold few events, so a linear scan of the
    // slot's output so far beats hashing.
    val n = merged.length
    val outIters = new Array[Int](n)
    val outDays = new Array[Int](n)
    val outAmts = new Array[Double](n)
    val outIds = new Array[Long](n)
    val outSrcs = new Array[Int](n)
    var m = 0
    var i = 0
    while i < n do
      val iter = merged.iters(i)
      val day = merged.days(i)
      val slotStart = m
      while i < n && merged.iters(i) == iter && merged.days(i) == day do
        val id = merged.ids(i)
        var found = -1
        var p = slotStart
        while found < 0 && p < m do
          if outIds(p) == id then found = p
          end if
          p += 1
        end while
        if found < 0 then
          outIters(m) = iter
          outDays(m) = day
          outAmts(m) = merged.amts(i)
          outIds(m) = id
          outSrcs(m) = merged.srcs(i)
          m += 1
        else
          outAmts(found) += merged.amts(i)
          parent(find(outSrcs(found))) = find(merged.srcs(i))
        end if
        i += 1
      end while
    end while

    // An ID can only have been summed within its slot, so a repeat now means it sits at two (iteration, day)s.
    val idCheck = java.util.Arrays.copyOf(outIds, m)
    java.util.Arrays.sort(idCheck)
    var q = 1
    while q < m do
      require(
        idCheck(q) != idCheck(q - 1),
        s"Event with ID ${idCheck(q)} appears at more than one (iteration, day)"
      )
      q += 1
    end while

    val groupThreshold = new Array[Double](k)
    var src = 0
    while src < k do
      groupThreshold(find(src)) += withEvents(src).threshold
      src += 1
    end while
    val eventsThreshold = (0 until k).iterator.filter(s => find(s) == s).map(groupThreshold(_)).max
    val newThreshold = emptyThreshold.fold(eventsThreshold)(math.max(_, eventsThreshold))

    // Filter in place, keeping order.
    var w = 0
    var r = 0
    while r < m do
      if outAmts(r) > newThreshold then
        outIters(w) = outIters(r)
        outDays(w) = outDays(r)
        outAmts(w) = outAmts(r)
        outIds(w) = outIds(r)
        w += 1
      end if
      r += 1
    end while

    new Scenarr(
      java.util.Arrays.copyOf(outIters, w),
      java.util.Arrays.copyOf(outDays, w),
      java.util.Arrays.copyOf(outAmts, w),
      java.util.Arrays.copyOf(outIds, w),
      ref.numberIterations,
      newThreshold,
      ref.day1,
      s"concat: [${scenarios.map(_.name).mkString(" + ")}]",
      scala.util.Random.nextLong(),
      isSorted = true
    )
  end combineAll

  /** Infix operator for combining scenarios */
  extension (s1: Scenarr) def |+|(s2: Scenarr): Scenarr = combine(s1, s2)
  end extension

  /** Event columns sorted by (iteration, day), each event tagged with the index of the scenario it came from. */
  private final class Cols(
      val iters: Array[Int],
      val days: Array[Int],
      val amts: Array[Double],
      val ids: Array[Long],
      val srcs: Array[Int]
  ):
    inline def length: Int = ids.length
  end Cols

  private def mergeAll(cs: IndexedSeq[Cols]): Cols =
    if cs.length == 1 then cs.head
    else
      val mid = cs.length / 2
      merge2(mergeAll(cs.take(mid)), mergeAll(cs.drop(mid)))

  /** Stable merge of two (iteration, day)-sorted column sets. */
  private def merge2(a: Cols, b: Cols): Cols =
    val n = a.length + b.length
    val out = Cols(new Array[Int](n), new Array[Int](n), new Array[Double](n), new Array[Long](n), new Array[Int](n))
    var i = 0
    var j = 0
    var o = 0
    while o < n do
      val takeA =
        j >= b.length || (i < a.length &&
          (a.iters(i) < b.iters(j) || (a.iters(i) == b.iters(j) && a.days(i) <= b.days(j))))
      if takeA then
        copyEvent(a, i, out, o)
        i += 1
      else
        copyEvent(b, j, out, o)
        j += 1
      end if
      o += 1
    end while
    out
  end merge2

  private inline def copyEvent(from: Cols, at: Int, to: Cols, o: Int): Unit =
    to.iters(o) = from.iters(at)
    to.days(o) = from.days(at)
    to.amts(o) = from.amts(at)
    to.ids(o) = from.ids(at)
    to.srcs(o) = from.srcs(at)
  end copyEvent

  /** Create a Scenarr with automatically generated random IDs for each event. Use this factory when you don't need to
    * specify event IDs explicitly.
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
        scenario.iterations.mask(idx),
        scenario.days.mask(idx),
        scenario.amounts.mask(idx),
        scenario.ids.mask(idx),
        scenario.numberIterations,
        newThresh,
        scenario.day1,
        scenario.name
      )
    end applyThreshold
  end extension
end Scenarr
