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

  /** Aggregate exceedance curve: the total of all events per iteration, ranked. Iterations with no events count as 0.
    */
  lazy val aep: ExceedanceCurve = ExceedanceCurve.fromIterationValues(agg)

  /** Occurrence exceedance curve: the largest single event per iteration, ranked. Iterations with no events count as a
    * largest loss of 0.
    *
    * Only empty iterations (left at -Inf by `groupMax`) are replaced. A negative largest loss is kept as is, so don't
    * simplify this to `math.max(_, 0)`: negative losses may become meaningful depending on how inuring is treated.
    */
  lazy val oep: ExceedanceCurve =
    assert(isSorted, "Scenario must be sorted to compute occurrence exceedance")
    val maxes = groupMax(iterations, amounts, numberIterations)
    var i = 0
    while i < maxes.length do
      if maxes(i) == Double.NegativeInfinity then maxes(i) = 0d
      end if
      i += 1
    end while
    ExceedanceCurve.fromIterationValues(maxes)
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

  /** One line, cheap: doesn't sort or compute any statistics. Use [[show]] for a report. */
  override def toString: String =
    val label = if name.isEmpty then "" else s"\"$name\", "
    s"Scenarr(${label}events=${amounts.length}, numberIterations=$numberIterations, threshold=${Scenarr.fmtAmount(threshold)}, day1=$day1, isSorted=$isSorted, id=$id)"
  end toString

  /** A multi-line, terminal-friendly report with the first 10 events. See `show(eventRows)`. */
  def show: String = show(10)

  /** A multi-line, terminal-friendly report: summary statistics, OEP / AEP at standard return periods (those the
    * simulation can support), the largest events, and the first `eventRows` events in (iteration, day) order.
    *
    * Sorts a copy if this scenario isn't flagged sorted, so it costs O(n log n) on unsorted data. Meant for
    * interrogating a scenario by eye, not for parsing.
    */
  def show(eventRows: Int): String = Scenarr.report(this, eventRows)

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
    *
    * '''Event IDs from more than one source.''' The same-ID-means-same-event rule is easy to trip over: vendor event
    * IDs are usually only unique within one model or catalogue, so two perils' scenarios can share an ID by
    * coincidence. Their losses are then summed into one event (inflating the OEP), the scenarios are joined into one
    * threshold group, and if the two events sit on different days `combineAll` throws. Build IDs with
    * [[Scenarr.eventKey]] (or remap with `withIdSource`) so that only a genuinely shared event shares an ID.
    *
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

  private[vecxt_re] def fmtAmount(x: Double): String = f"$x%,.2f"

  private val standardReturnPeriods = Array(2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 5000, 10000)

  /** Pads columns to a common width; numeric-looking columns are right aligned. */
  private def table(header: Seq[String], rows: Seq[Seq[String]], indent: String): Seq[String] =
    val widths = header.indices.map(c => (header(c) +: rows.map(_(c))).map(_.length).max)
    def line(cells: Seq[String]) =
      cells.indices.map(c => cells(c).reverse.padTo(widths(c), ' ').reverse).mkString(indent, " | ", "")
    (line(header) +: widths.map("-" * _).mkString(indent, "-+-", "") +: rows.map(line)).toSeq
  end table

  private def report(scenario: Scenarr, eventRows: Int): String =
    val s = scenario.sorted
    val n = s.amounts.length
    val ni = s.numberIterations
    val nl = "\n"
    val out = scala.collection.mutable.ArrayBuffer.empty[String]

    val label = if s.name.isEmpty then "Scenarr" else s"Scenarr \"${s.name}\""
    out += s"$label (id ${s.id})"
    out += f"  events           $n%,d${
        if scenario.isSorted then "" else " (unsorted; statistics computed on a sorted copy)"
      }"
    out += f"  iterations       $ni%,d"
    out += s"  threshold        ${fmtAmount(s.threshold)}"
    out += s"  day1             ${s.day1}"

    if n == 0 then
      out += "  (no events)"
      return out.mkString(nl)
    end if

    out += s"  period           ${s.period.firstLoss} .. ${s.period.lastLoss}"
    out += f"  freq / iteration mean ${n.toDouble / ni}%.4g, var/mean ${s.varianceMeanRatio}%.4g, cluster coeff ${s.clusterCoeff}%.4g"
    out += s"  total loss       ${fmtAmount(s.amounts.sum)}"
    out += s"  mean annual loss ${fmtAmount(s.meanLoss)}"
    out += s"  event amount     min ${fmtAmount(s.amounts.minSIMD)}, mean ${fmtAmount(s.amounts.sum / n)}, max ${fmtAmount(s.amounts.maxSIMD)}"

    val rps = standardReturnPeriods.filter(_ <= ni)
    if rps.nonEmpty then
      out += ""
      out ++= table(
        Seq("Return period", "OEP", "AEP"),
        rps.toSeq.map(rp => Seq(rp.toString, fmtAmount(s.oep.lossAt(rp)), fmtAmount(s.aep.lossAt(rp)))),
        "  "
      )
    end if

    def eventRow(i: Int) = Seq(
      s.iterations(i).toString,
      s.days(i).toString,
      s.day1.plusDays((s.days(i) - 1).toLong).toString,
      fmtAmount(s.amounts(i)),
      s.ids(i).toString
    )
    val eventHeader = Seq("Iteration", "Day", "Date", "Amount", "Id")

    // Top k by amount: k is small, so repeated selection beats sorting a large scenario.
    val k = math.min(5, n)
    val taken = new Array[Boolean](n)
    val largest = (0 until k).map { _ =>
      var best = -1
      var i = 0
      while i < n do
        if !taken(i) && (best < 0 || s.amounts(i) > s.amounts(best)) then best = i
        end if
        i += 1
      end while
      taken(best) = true
      best
    }
    out += ""
    out += s"  Largest $k events"
    out ++= table(eventHeader, largest.map(eventRow), "  ")

    val shown = math.min(math.max(eventRows, 0), n)
    if shown > 0 then
      out += ""
      out += s"  First $shown of $n events, by (iteration, day)"
      out ++= table(eventHeader, (0 until shown).map(eventRow), "  ")
    end if

    out.mkString(nl)
  end report

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

  /** A Scenarr event ID that is unique across sources: `source` in the high 32 bits, the source's own `eventId` in the
    * low 32.
    *
    * Use it when scenarios come from more than one model or vendor whose event IDs may collide (see `combineAll`). A
    * given (source, eventId) always maps to the same key, so the same event loaded from different loss tables (e.g. two
    * programmes on one catalogue) still lines up and is summed.
    */
  inline def eventKey(source: Int, eventId: Int): Long = (source.toLong << 32) | (eventId.toLong & 0xffffffffL)

  /** The `source` an [[eventKey]] was built from. */
  inline def eventKeySource(key: Long): Int = (key >>> 32).toInt

  /** The source's own `eventId` an [[eventKey]] was built from. */
  inline def eventKeyEventId(key: Long): Int = key.toInt

  /** A [[Builder]] for a scenario of `numberIterations` iterations. */
  def builder(
      numberIterations: Int,
      threshold: Double = 0d,
      day1: LocalDate = LocalDate.of(2019, 1, 1),
      name: String = "",
      id: Long = scala.util.Random.nextLong(),
      sizeHint: Int = 16
  ): Builder = Builder(numberIterations, threshold, day1, name, id, sizeHint)

  /** Collects events one at a time into primitive columns, then builds a [[Scenarr]]. Use it for streaming sources
    * (CSV, parquet, a database cursor), where the number of events isn't known up front.
    *
    * Each `add` checks the iteration lies in 1..numberIterations (the per-iteration kernels trust this without
    * checking), that the day is at least 1, and that the amount is not NaN. `result` checks the sort order in one pass:
    * events added in (iteration, day) order are used as they are, anything else is sorted. Either way the result has
    * `isSorted = true`.
    *
    * A builder can't be reused after `result`.
    */
  final class Builder(
      val numberIterations: Int,
      threshold: Double,
      day1: LocalDate,
      name: String,
      id: Long,
      sizeHint: Int
  ):
    require(numberIterations >= 1, s"numberIterations must be at least 1, got $numberIterations")
    private var its = new Array[Int](math.max(sizeHint, 1))
    private var ds = new Array[Int](its.length)
    private var amts = new Array[Double](its.length)
    private var eids = new Array[Long](its.length)
    private var n = 0
    private var ordered = true
    private var done = false

    /** The number of events added so far. */
    def length: Int = n

    def add(iteration: Int, day: Int, amount: Double, eventId: Long): this.type =
      if done then throw new IllegalStateException("Scenarr.Builder already built its result")
      end if
      if iteration < 1 || iteration > numberIterations then
        throw new IllegalArgumentException(
          s"Iteration $iteration outside 1..$numberIterations - is numberIterations the catalogue length?"
        )
      end if
      if day < 1 then throw new IllegalArgumentException(s"Day $day must be at least 1 (day 1 is day1)")
      end if
      if amount.isNaN then throw new IllegalArgumentException(s"NaN amount for event $eventId")
      end if
      if n == its.length then grow()
      end if
      if ordered && n > 0 then
        val prevIt = its(n - 1)
        ordered = iteration > prevIt || (iteration == prevIt && day >= ds(n - 1))
      end if
      its(n) = iteration
      ds(n) = day
      amts(n) = amount
      eids(n) = eventId
      n += 1
      this
    end add

    private def grow(): Unit =
      val cap = math.max(4, math.min(Int.MaxValue - 8L, its.length.toLong * 2).toInt)
      its = java.util.Arrays.copyOf(its, cap)
      ds = java.util.Arrays.copyOf(ds, cap)
      amts = java.util.Arrays.copyOf(amts, cap)
      eids = java.util.Arrays.copyOf(eids, cap)
    end grow

    def result(): Scenarr =
      if done then throw new IllegalStateException("Scenarr.Builder already built its result")
      end if
      done = true
      val s = new Scenarr(
        if n == its.length then its else java.util.Arrays.copyOf(its, n),
        if n == ds.length then ds else java.util.Arrays.copyOf(ds, n),
        if n == amts.length then amts else java.util.Arrays.copyOf(amts, n),
        if n == eids.length then eids else java.util.Arrays.copyOf(eids, n),
        numberIterations,
        threshold,
        day1,
        name,
        id,
        isSorted = ordered
      )
      if ordered then s else s.sorted
      end if
    end result
  end Builder

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

  /** True when events are already in (iteration, day) order. One pass, no allocation. */
  private[vecxt_re] def isOrderedByIterationDay(iterations: Array[Int], days: Array[Int]): Boolean =
    var i = 1
    while i < iterations.length do
      val it0 = iterations(i - 1)
      val it1 = iterations(i)
      if it1 < it0 || (it1 == it0 && days(i) < days(i - 1)) then return false
      end if
      i += 1
    end while
    true
  end isOrderedByIterationDay

  /** The permutation that stably sorts events by (iteration, day): `perm(k)` is the original index of the k-th event.
    * Events with equal (iteration, day) keep their original relative order.
    *
    * Iterations are dense (1..numberIterations) and days span about a year, so both keys usually have a small range.
    * Then this is an LSD radix sort: a stable counting sort by day, followed by a stable counting sort by iteration.
    * That's O(n + iteration range + day range), with primitive arrays only.
    *
    * If either key's range is much larger than the number of events (e.g. a handful of events spread over millions of
    * iterations, or unusual values), the counting arrays would be wasteful, so it falls back to a stable merge sort.
    * Any Int values are accepted, as the original comparison sort did.
    */
  private[vecxt_re] def sortPermutation(iterations: Array[Int], days: Array[Int]): Array[Int] =
    val n = iterations.length
    if n == 0 then Array.emptyIntArray
    else
      var minIt = iterations(0)
      var maxIt = minIt
      var minDay = days(0)
      var maxDay = minDay
      var i = 1
      while i < n do
        val it = iterations(i)
        if it < minIt then minIt = it
        else if it > maxIt then maxIt = it
        end if
        val d = days(i)
        if d < minDay then minDay = d
        else if d > maxDay then maxDay = d
        end if
        i += 1
      end while
      val itRange = maxIt.toLong - minIt + 1
      val dayRange = maxDay.toLong - minDay + 1
      // Counting arrays this size are cheap relative to the arrays being sorted
      val maxRange = math.max(4L * n, 1L << 16)
      if itRange <= maxRange && dayRange <= maxRange then
        val byDay = countingSortPermutation(null, days, minDay, dayRange.toInt)
        countingSortPermutation(byDay, iterations, minIt, itRange.toInt)
      else mergeSortPermutation(iterations, days)
      end if
    end if
  end sortPermutation

  /** Stable counting sort of the events listed in `order` (or 0 until n when `order` is null) by `keys`, whose values
    * lie in [min, min + range). Returns the reordered indices.
    */
  private[vecxt_re] def countingSortPermutation(
      order: Array[Int] | Null,
      keys: Array[Int],
      min: Int,
      range: Int
  ): Array[Int] =
    val n = keys.length
    // starts(k) = first output slot for key bucket k
    val starts = new Array[Int](range + 1)
    var i = 0
    while i < n do
      starts(keys(i) - min + 1) += 1
      i += 1
    end while
    var k = 1
    while k <= range do
      starts(k) += starts(k - 1)
      k += 1
    end while
    val out = new Array[Int](n)
    i = 0
    if order == null then
      while i < n do
        val b = keys(i) - min
        out(starts(b)) = i
        starts(b) += 1
        i += 1
      end while
    else
      val ord = order.nn
      while i < n do
        val idx = ord(i)
        val b = keys(idx) - min
        out(starts(b)) = idx
        starts(b) += 1
        i += 1
      end while
    end if
    out
  end countingSortPermutation

  /** Stable bottom-up merge sort of event indices by (iteration, day). The fallback for sparse key ranges. */
  private[vecxt_re] def mergeSortPermutation(iterations: Array[Int], days: Array[Int]): Array[Int] =
    val n = iterations.length
    var src = new Array[Int](n)
    var i = 0
    while i < n do
      src(i) = i
      i += 1
    end while
    var dst = new Array[Int](n)
    var width = 1
    while width < n do
      var lo = 0
      while lo < n do
        val mid = math.min(lo + width, n)
        val hi = math.min(lo.toLong + 2L * width, n.toLong).toInt
        var a = lo
        var b = mid
        var o = lo
        while o < hi do
          // Take from the left run unless the right element is strictly smaller: keeps the sort stable
          val takeLeft = a < mid && (b >= hi || {
            val ia = src(a)
            val ib = src(b)
            val ita = iterations(ia)
            val itb = iterations(ib)
            !(itb < ita || (itb == ita && days(ib) < days(ia)))
          })
          if takeLeft then
            dst(o) = src(a)
            a += 1
          else
            dst(o) = src(b)
            b += 1
          end if
          o += 1
        end while
        lo = hi
      end while
      val t = src
      src = dst
      dst = t
      width = if width.toLong * 2 >= n then n else width * 2
    end while
    src
  end mergeSortPermutation

  extension (scenario: Scenarr)
    /** This scenario ordered by (iteration, day), with `isSorted = true`. Events on the same iteration and day keep
      * their original relative order.
      *
      * Returns `scenario` itself when it is already flagged sorted. When it is in order but not flagged, the result
      * shares its arrays (no copy). Otherwise the columns are copied in sorted order.
      */
    def sorted: Scenarr =
      if scenario.isSorted then scenario
      else if isOrderedByIterationDay(scenario.iterations, scenario.days) then scenario.copy(isSorted = true)
      else
        val perm = sortPermutation(scenario.iterations, scenario.days)
        Scenarr(
          scenario.iterations.select(perm),
          scenario.days.select(perm),
          scenario.amounts.select(perm),
          scenario.ids.select(perm),
          scenario.numberIterations,
          scenario.threshold,
          scenario.day1,
          scenario.name,
          scenario.id,
          isSorted = true
        )
      end if
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

    /** This scenario with every event ID replaced by `eventKey(source, id)`, so its IDs can't collide with those of a
      * scenario from another source under `combineAll`.
      *
      * @throws IllegalArgumentException
      *   if an ID doesn't fit in 32 bits (it already carries a source, or isn't a vendor event ID)
      */
    def withIdSource(source: Int): Scenarr =
      val out = new Array[Long](scenario.ids.length)
      var i = 0
      while i < out.length do
        val raw = scenario.ids(i)
        require(raw == raw.toInt.toLong, s"Event ID $raw doesn't fit in 32 bits, so it can't take a source")
        out(i) = eventKey(source, raw.toInt)
        i += 1
      end while
      scenario.copy(ids = out)
    end withIdSource

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
        scenario.name,
        scenario.id,
        isSorted = scenario.isSorted // masking preserves order
      )
    end applyThreshold
  end extension
end Scenarr
