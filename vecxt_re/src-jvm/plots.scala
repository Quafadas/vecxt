package vecxt_re

import io.github.quafadas.plots.SetupVega.{*, given}

object Plots:
  lazy val timeline = VegaPlot.fromResource("timeline.vl.json") // riskInceptionDate, riskExpiryDate
  lazy val seasonality = VegaPlot.fromResource("seasonality.vg.json") // catagory, amount
end Plots
