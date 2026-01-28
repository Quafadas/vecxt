package vecxt_re

import io.circe.syntax.*
import io.github.quafadas.plots.SetupVega.{*, given}

object Plots:
  // These must be private otherwise scaladoc get crazy.
  private lazy val timeline = VegaPlot.fromResource("timeline.vl.json") // riskInceptionDate, riskExpiryDate
  private lazy val seasonality = VegaPlot.fromResource("seasonality.vg.json") // catagory, amount
  private lazy val distributionDensity = VegaPlot.fromResource("distDensity.vg.json") // value, density

  extension (idx: CalendarYearIndex)
    def plotIndex(reportingThreshold: Double)(using viz.LowPriorityPlotTarget) =
      val linePlot2 = VegaPlot.fromResource("index.vl.json")
      val cumulative = idx.onLevel(Array.fill(idx.years.length)(1.0), idx.years)
      val factors = idx.years.zip(idx.indices).zip(cumulative).map { case ((year, index), cumulative) =>
        (
          year = year,
          index = index,
          missing = 1 / cumulative,
          threshold = idx.suggestedNewThreshold(reportingThreshold)
        )
      }
      linePlot2.plot(
        _.data.values := factors.asJson
      )
  end extension

  extension (scenario: Scenarr)
    inline def plotSeasonality(highlight: Option[(year: Int, month: Int)] = None)(using
        tgt: viz.LowPriorityPlotTarget
    ) =
      val calc = scenario.monthYear.zip(scenario.amounts).groupMapReduce(_._1)(_._2)(_ + _).toVector
      val normaliseBy = calc.map(_._2).sum // total of all claims
      val sorted = calc
        .sortBy(row => (row._1.year, row._1.month))
        .map(row =>
          (
            category =
              s"${row._1.month.getDisplayName(java.time.format.TextStyle.SHORT, java.util.Locale.getDefault())} ${row._1.year}",
            amount = row._2 / normaliseBy,
            color = highlight.exists(h => h.year == row._1.year && h.month == row._1.month.getValue)
          )
        )

      seasonality.plot(
        _.title("Seasonality " + scenario.name),
        _.data.values := sorted.asJson
      )
  end extension

  // extension (negBin: NegativeBinomial)
  //   inline def plotPdf(using viz.LowPriorityPlotTarget) =
  //     val numPoints = 1000
  //     val maxX = negBin.mean + 4 * math.sqrt(negBin.variance)
  //     val data = (0 until numPoints).map { i =>
  //       val x = i.toDouble * maxX / numPoints
  //       (value = x, density = negBin.probabilityOf(x.round.toInt))
  //     }

  //     distributionDensity.plot(
  //       _.title(s"Negative Binomial Distribution Density (a=${negBin.a}, b=${negBin.b})"),
  //       _.data.values := data.asJson
  //     )

  //   inline def plotCdf(using viz.LowPriorityPlotTarget) =
  //     val numPoints = 1000
  //     val maxX = negBin.mean + 4 * math.sqrt(negBin.variance)
  //     val data = (0 until numPoints).map { i =>
  //       val x = i.toDouble * maxX / numPoints
  //       (value = x, density = negBin.cdf(x))
  //     }

  //     distributionDensity.plot(
  //       _.title(s"Negative Binomial Distribution CDF (a=${negBin.a}, b=${negBin.b})"),
  //       _.data.values := data.asJson
  //     )
  // end extension
end Plots
