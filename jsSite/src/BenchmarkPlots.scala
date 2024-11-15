package vecxt.plot

import scala.NamedTuple.AnyNamedTuple
import scala.annotation.meta.field
import upickle.default.*
import NamedTupleReadWriter.given

object BenchmarkPlots:
  def addScalarBenchmark: String =
    val thePlot = BenchmarkPlotElements.schema ++
      BenchmarkPlotElements.data("../../benchmarks/benchmark_history.json") ++
      // BenchmarkPlotElements.fakeData ++
      (transform =
        BenchmarkPlotElements.transform(List("AddScalarBenchmark.vecxt_add", "AddScalarBenchmark.vecxt_add_vec"))
      ) ++
      (vconcat =
        List(
          BenchmarkPlotElements.layer(10),
          BenchmarkPlotElements.layer(1000),
          BenchmarkPlotElements.layer(100000)
        )
      )
    write(thePlot)
  end addScalarBenchmark

  def addScalarBenchmarkOverTime: String =
    val thePlot = BenchmarkPlotElements.schema ++
      // BenchmarkPlotElements.data("../../benchmarks/benchmark_history.json") ++
      BenchmarkPlotElements.fakeData ++
      (transform = BenchmarkPlotElements.timeTransform(List("AddScalarBenchmark.vecxt_add_vec"))) ++
      (vconcat =
        List(
          BenchmarkPlotElements.timeLayer(10),
          BenchmarkPlotElements.timeLayer(1000),
          BenchmarkPlotElements.timeLayer(100000)
        )
      )
    write(thePlot)
  end addScalarBenchmarkOverTime

  def countTrueBenchmark =
    val thePlot = BenchmarkPlotElements.schema ++
      BenchmarkPlotElements.data("../../benchmarks/benchmark_history.json") ++
      (transform =
        BenchmarkPlotElements.transform(
          List("CountTrueBenchmark.countTrue_loop", "CountTrueBenchmark.countTrue_loop_vec")
        )
      ) ++
      (vconcat =
        List(
          BenchmarkPlotElements.layer(3),
          BenchmarkPlotElements.layer(128),
          BenchmarkPlotElements.layer(100000)
        )
      )
    write(thePlot)
  end countTrueBenchmark

end BenchmarkPlots
