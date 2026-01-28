package experiments

import io.github.quafadas.table.{*, given}
import io.github.quafadas.plots.SetupVegaBrowser.{*, given}
import experiments.RPT.*

@main def plotIndex =
  val idx = CSV.resource("idx.csv", CsvOpts(TypeInferrer.FromAllRows, ReadAs.Columns))
  val calYrIdx = vecxt_re.CalendarYearIndex(2025, idx.year, idx.idx)
  println(calYrIdx)
  calYrIdx.plotIndex(1.0)
  println("finished")
end plotIndex
