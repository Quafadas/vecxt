package vecxt_re

import io.github.quafadas.plots.SetupVega.*

object Plots:
  lazy val timeline = VegaPlot.fromResource("timeline.vl.json") // riskInceptionDate, riskExpiryDate
  lazy val seasonality = VegaPlot.fromResource("seasonality.vg.json") // catagory, amount
  lazy val distributionDensity = VegaPlot.fromResource("distDensity.vg.json") // value, density
end Plots
