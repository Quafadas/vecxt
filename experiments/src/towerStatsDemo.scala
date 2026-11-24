import vecxt_re.all.*
import vecxt_re.ReportDenominator
import io.github.quafadas.table.{*, given}

/** Sketch: simulate a catalogue, push it through an XoL tower, report per-layer stats. Illustrative numbers only. */
@main def towerStatsDemo(): Unit =
  val numIterations = 10_000

  // 1. Scenarios: Poisson(3) claims/year, empirical body (in m) with a Pareto tail above 10m.
  val severity = Mixed.fromWeightedValues(
    values = Array(2.0, 4.0, 6.0, 8.0, 10.0),
    weights = Array(4.0, 3.0, 2.0, 1.0, 0.5),
    mixingPoint = 10.0,
    paretoShape = 1.8
  )
  val scenario = FreqSev(Poisson(3.0), severity).drawScenarr(numIterations, name = "Cat").sorted

  // 2. Tower: 10 xs 5, 20 xs 15, 30 xs 35 (occurrence terms), each with one reinstatement at 100%.
  //    Unnamed layers get Layer.autoName ("10.0 xs 5.0") in the loss report.
  val tower = Tower.oneAt100(ret = 5.0, limits = Vector(10.0, 20.0, 30.0)).copy(name = Some("Cat XoL"))
  println(tower.show)
  println()

  // 3. Split: per-event ceded to each layer, plus total ceded / retained.
  val split = tower.splitScenarioAmounts(scenario)
  val years = scenario.iterations // valid because scenario is sorted

  // 4. Per-layer stats via ReReporting.lossReport: one single pass (Welford) per layer, giving
  //    (name, limit, el, stdDev, attachProb, exhaustProb) as a named tuple; ptbln renders the lot as a table.
  //    FirstLimit: el / stdDev as a fraction of limit * share. Custom(1.0): raw ceded amounts.
  println("Loss report - fraction of occ limit")
  split.splits.map(_.lossReport(numIterations, years, ReportDenominator.FirstLimit)).ptbln
  println("Loss report - raw amounts")
  split.splits.map(_.lossReport(numIterations, years, ReportDenominator.Custom(1.0))).ptbln
  println()

  // 5. Gross vs ceded vs net exceedance curves.
  //    The split keeps the sorted event order, so swapping amounts on the scenario gives ceded / net views.
  val gross = scenario.aep
  val ceded = scenario.copy(amounts = split.ceded).aep
  val net = scenario.copy(amounts = split.retained).aep

  println(f"${"RP"}%6s ${"Gross AEP"}%10s ${"Ceded"}%10s ${"Net"}%10s ${"Gross OEP"}%10s")
  Seq(2, 5, 10, 25, 50, 100, 250, 1000).foreach { rp =>
    println(
      f"$rp%6d ${gross.lossAt(rp)}%10.2f ${ceded.lossAt(rp)}%10.2f ${net.lossAt(rp)}%10.2f ${scenario.oep.lossAt(rp)}%10.2f"
    )
  }
end towerStatsDemo
