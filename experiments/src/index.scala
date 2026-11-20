package experiments

import io.github.quafadas.table.{*, given}
import io.github.quafadas.plots.SetupVegaBrowser.{*, given}
import experiments.RPT.*

@main def plotIndex =
  val idx = CSV.resource("idx.csv", CsvOpts(TypeInferrer.FromAllRows, ReadAs.Columns))
  val calYrIdx = vecxt_re.CalendarYearIndex(idx.year, idx.idx)
  println(calYrIdx)
  calYrIdx.plotIndex(1.0, calYrIdx.earliestYear)
  println("finished")
end plotIndex
