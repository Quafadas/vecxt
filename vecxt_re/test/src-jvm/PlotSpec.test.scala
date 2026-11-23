package vecxt_re

import java.time.LocalDate

import munit.FunSuite
import vecxt_re.HillEstimator.HillPlotResult
import vecxt_re.all.*

/** Every plot, rendered into a target that captures the final spec, must pass [[SpecCheck]]: each mark has data, and
  * every field it uses is in that data.
  */
class PlotSpecTest extends FunSuite:

  private final class Capture extends viz.LowPriorityPlotTarget:
    var specs = Vector.empty[ujson.Value]
    override def show(spec: ujson.Value, lib: viz.ChartLibrary): Unit = specs :+= spec
  end Capture

  private def specOf(plot: viz.LowPriorityPlotTarget ?=> Any): ujson.Value =
    val c = Capture()
    plot(using c)
    assertEquals(c.specs.length, 1, "expected one spec")
    c.specs.head
  end specOf

  private def plotTest(name: String)(plot: viz.LowPriorityPlotTarget ?=> Any)(using munit.Location): Unit =
    test(s"$name spec is consistent with its data") {
      val problems = SpecCheck.problems(specOf(plot))
      assert(problems.isEmpty, problems.mkString("\n", "\n", ""))
    }

  // Deterministic inputs
  private val paretoSample = Array.tabulate(60)(i => 1e6 / math.pow((i + 0.5) / 60, 1 / 2.5))
  private val counts = IndexedSeq(1, 0, 2, 3, 1, 0, 1, 4, 2, 1, 0, 2)
  private val years = 2010 until 2010 + counts.length
  private val lossYears = Vector(2015, 2016, 2016, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
  private val losses = Vector(3.1e6, 1.2e6, 2.2e6, 4.0e6, 1.5e6, 2.7e6, 6.1e6, 1.9e6, 2.4e6, 3.3e6)
  private val dates = lossYears.zipWithIndex.map((y, i) => LocalDate.of(y, 1 + i % 12, 10))

  plotTest("CalendarYearRateChanges.plotIndex")(
    CalendarYearRateChanges(Array(2020, 2021, 2022), Array(1.05, 1.03, 1.04)).plotIndex(1e6, 2020)
  )
  plotTest("CalendarYearLevels.plotLevels")(
    CalendarYearLevels(Array(2020, 2021, 2022), Array(1.0, 1.05, 1.1)).plotLevels()
  )

  plotTest("NegativeBinomial.plot")(NegativeBinomial(2.0, 1.5).plot)
  plotTest("NegativeBinomial.plotCdf")(NegativeBinomial(2.0, 1.5).plotCdf)
  plotTest("NegativeBinomial.plotEcdfVsCdf")(NegativeBinomial(2.0, 1.5).plotEcdfVsCdf(counts))
  plotTest("NegativeBinomial.plotRootogram")(NegativeBinomial(2.0, 1.5).plotRootogram(counts))
  plotTest("NegativeBinomial.plotPearsonResiduals")(NegativeBinomial(2.0, 1.5).plotPearsonResiduals(counts))

  plotTest("Poisson.plot")(Poisson(1.5).plot)
  plotTest("Poisson.plotCdf")(Poisson(1.5).plotCdf)
  plotTest("Poisson.plotEcdfVsCdf")(Poisson(1.5).plotEcdfVsCdf(counts))
  plotTest("Poisson.plotRootogram")(Poisson(1.5).plotRootogram(counts))
  plotTest("Poisson.plotPearsonResiduals")(Poisson(1.5).plotPearsonResiduals(counts))
  plotTest("Poisson.plotTrend")(Poisson(1.5).plotTrend(years, counts))

  plotTest("Pareto.plot")(Pareto(1e6, 2.5).plot)
  plotTest("Pareto.plotCdf")(Pareto(1e6, 2.5).plotCdf)
  plotTest("Empirical.plot")(Empirical.equalWeights(paretoSample).plot)
  plotTest("Empirical.plotCdf")(Empirical.equalWeights(paretoSample).plotCdf)

  private val mixed = Mixed.fromValues(paretoSample, mixingPoint = 3e6, paretoShape = 2.5)
  plotTest("Mixed.plot")(mixed.plot)
  plotTest("Mixed.plotCdf")(mixed.plotCdf)
  plotTest("Mixed.plotLogLogVsSample")(mixed.plotLogLogVsSample(paretoSample.toIndexedSeq, threshold = 1e6))

  plotTest("Scenarr.plotSeasonality")(
    new Scenarr(Array(1, 1, 1), Array(10, 40, 200), Array(100.0, 200.0, 50.0), Array(1L, 2L, 3L), numberIterations = 1)
      .plotSeasonality(highlight = Some((year = 2019, month = 2)))
  )

  plotTest("plotLossStems")(plotLossStems(dates, losses.map(_ * 0.8), losses, rollingYears = 3))
  plotTest("plotSeverityTrend")(plotSeverityTrend(lossYears, losses))

  plotTest("HillPlotResult.plotHill")(HillEstimator.hillPlot(paretoSample).plotHill)
  HillConvention.values.foreach { c =>
    plotTest(s"HillPlotResult.plotHill $c")(HillEstimator.hillPlot(paretoSample, convention = c).plotHill)
  }

  private def layerValues(spec: ujson.Value, layer: Int) = spec("layer")(layer)("data")("values").arr
  private def hillSpec(c: HillConvention) = specOf(
    HillEstimator.hillPlot(paretoSample, kMin = 2, convention = c).plotHill
  )

  test("HillPlotResult.plotHill band is the same for MaxLikelihood and Unbiased, shifted by one loss for Embrechts") {
    def band(spec: ujson.Value) = layerValues(spec, 0).map(v => (v("x").num, v("lower").num, v("upper").num))
    val mle = band(hillSpec(HillConvention.MaxLikelihood))
    val unbiased = band(hillSpec(HillConvention.Unbiased))
    val ekm = band(hillSpec(HillConvention.Embrechts))
    assertEquals(unbiased.length, mle.length)
    mle.zip(unbiased).foreach { case ((x1, l1, u1), (x2, l2, u2)) =>
      assertEqualsDouble(x1, x2, 1e-12)
      assertEqualsDouble(l1, l2, 1e-9)
      assertEqualsDouble(u1, u2, 1e-9)
    }
    // Embrechts at k is the maximum likelihood band at k - 1, one step to the right
    val step = 1.0 / paretoSample.length
    mle.zip(ekm.tail).foreach { case ((x1, l1, u1), (x2, l2, u2)) =>
      assertEqualsDouble(x2, x1 + step, 1e-12)
      assertEqualsDouble(l1, l2, 1e-9)
      assertEqualsDouble(u1, u2, 1e-9)
    }
  }

  test("HillPlotResult.plotHill points sit at the bottoms of the teeth, or the tops for Embrechts") {
    def curve(c: HillConvention) = layerValues(hillSpec(c), 2).map(v => (v("x").num, v("estimate").num))
    def points(c: HillConvention) = layerValues(hillSpec(c), 1).map(v => (v("x").num, v("estimate").num))
    def near(xs: Iterable[(Double, Double)], x: Double) = xs.filter((cx, _) => math.abs(cx - x) < 1e-12).map(_._2)
    for c <- HillConvention.values do
      val cv = curve(c)
      points(c).foreach { (x, est) =>
        val ys = near(cv, x)
        assert(ys.nonEmpty, s"$c: no curve point at $x")
        if c == HillConvention.Embrechts then assertEqualsDouble(ys.max, est, 1e-9)
        else assertEqualsDouble(ys.min, est, 1e-9)
        end if
      }
    end for
    // Embrechts is the maximum likelihood curve shifted right by one loss: Embrechts teeth k = 4..31 are the
    // maximum likelihood teeth k = 3..30, sampled at the same thresholds
    val step = 1.0 / paretoSample.length
    def sawtooth(c: HillConvention, kMax: Int) =
      layerValues(specOf(HillEstimator.hillPlot(paretoSample, kMin = 2, kMax = kMax, convention = c).plotHill), 2)
        .map(v => (v("x").num, v("estimate").num))
    val ekm = sawtooth(HillConvention.Embrechts, 31)
    sawtooth(HillConvention.MaxLikelihood, 30).foreach { (x, y) =>
      assert(near(ekm, x + step).exists(e => math.abs(e - y) < 1e-9), s"no Embrechts curve point at ($x + 1/n, $y)")
    }
  }

  plotTest("HillPlotResult.plotHill without thresholds or n") {
    val full = HillEstimator.hillPlot(paretoSample)
    HillPlotResult(full.kValues, full.estimates).plotHill
  }
  plotTest("PickandsPlotResult.plotPickands")(PickandsEstimator.pickandsPlot(paretoSample).plotPickands)
  plotTest("PickandsPlotResult.plotPickandsGamma")(PickandsEstimator.pickandsPlot(paretoSample).plotPickandsGamma)

  // The checker itself

  test("SpecCheck reports a field missing from the data (the Pickands bug)") {
    val spec = ujson.read("""{
      "data": {"values": [{"k": 1, "estimate": 2.0}]},
      "encoding": {"x": {"field": "threshold", "type": "quantitative"}},
      "layer": [{"mark": "line", "encoding": {"y": {"field": "estimate", "type": "quantitative"}}}]
    }""")
    assertEquals(SpecCheck.problems(spec), List("spec.layer[0]: x uses field 'threshold', missing from 1 of 1 rows"))
  }

  test("SpecCheck accepts fields made by transforms, and checks filter expressions") {
    val spec = ujson.read("""{
      "data": {"values": [{"a": 1}]},
      "transform": [{"calculate": "datum.a * 2", "as": "b"}, {"filter": "datum.c > 0"}],
      "mark": "point",
      "encoding": {"x": {"field": "b", "type": "quantitative"}}
    }""")
    assertEquals(SpecCheck.problems(spec), List("spec: transform uses field 'c', missing from 1 of 1 rows"))
  }

  test("SpecCheck reports empty data, a missing dataset, and an all-null quantitative field") {
    val spec = ujson.read("""{
      "datasets": {"d": [{"y": null}]},
      "layer": [
        {"data": {"values": []}, "mark": "line"},
        {"data": {"name": "nope"}, "mark": "line"},
        {"data": {"name": "d"}, "mark": "line", "encoding": {"y": {"field": "y", "type": "quantitative"}}}
      ]
    }""")
    assertEquals(
      SpecCheck.problems(spec),
      List(
        "spec.layer[0]: no data rows",
        "spec.layer[1]: data refers to dataset 'nope', which is not in datasets",
        "spec.layer[1]: no data rows",
        "spec.layer[2]: y field 'y' is null in every row"
      )
    )
  }

end PlotSpecTest
