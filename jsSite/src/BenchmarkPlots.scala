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
          BenchmarkPlotElements.layer(10, "n"),
          BenchmarkPlotElements.layer(1000, "n"),
          BenchmarkPlotElements.layer(100000, "n")
        )
      )
    write(thePlot)
  end addScalarBenchmark

  def addScalarBenchmarkOverTime: String =
    val thePlot = BenchmarkPlotElements.schema ++
      BenchmarkPlotElements.data("../../benchmarks/benchmark_history.json") ++
      // BenchmarkPlotElements.fakeData ++
      (transform = BenchmarkPlotElements.timeTransform(List("AddScalarBenchmark.vecxt_add_vec"))) ++
      (vconcat =
        List(
          BenchmarkPlotElements.timeLayer(10, "n"),
          BenchmarkPlotElements.timeLayer(1000, "n"),
          BenchmarkPlotElements.timeLayer(100000, "n")
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
          BenchmarkPlotElements.layer(3, "len"),
          BenchmarkPlotElements.layer(128, "len"),
          BenchmarkPlotElements.layer(100000, "len")
        )
      )
    write(thePlot)
  end countTrueBenchmark

  def andBooleanBenchmark =
    val thePlot = BenchmarkPlotElements.schema ++
      BenchmarkPlotElements.data("../../benchmarks/benchmark_history.json") ++
      (transform =
        BenchmarkPlotElements.transform(
          List("AndBooleanBenchmark.and_loop", "AndBooleanBenchmark.and_loop_vec")
        )
      ) ++
      (vconcat =
        List(
          BenchmarkPlotElements.layer(3, "len"),
          BenchmarkPlotElements.layer(128, "len"),
          BenchmarkPlotElements.layer(100000, "len")
        )
      )
    write(thePlot)
  end andBooleanBenchmark

  def OrBooleanBenchmark =
    val thePlot = BenchmarkPlotElements.schema ++
      BenchmarkPlotElements.data("../../benchmarks/benchmark_history.json") ++
      (transform =
        BenchmarkPlotElements.transform(
          List("OrBooleanBenchmark.or_loop", "OrBooleanBenchmark.or_vec")
        )
      ) ++
      (vconcat =
        List(
          BenchmarkPlotElements.layer(3, "len"),
          BenchmarkPlotElements.layer(128, "len"),
          BenchmarkPlotElements.layer(100000, "len")
        )
      )
    write(thePlot)
  end OrBooleanBenchmark

  def lteBenchmark =
    val thePlot = BenchmarkPlotElements.schema ++
      BenchmarkPlotElements.data("../../benchmarks/benchmark_history.json") ++
      (transform =
        BenchmarkPlotElements.transform(
          List("LogicalBenchmark.lte_vec", "LogicalBenchmark.lte_loop")
        )
      ) ++
      (vconcat =
        List(
          BenchmarkPlotElements.layer(3, "len"),
          BenchmarkPlotElements.layer(128, "len"),
          BenchmarkPlotElements.layer(100000, "len")
        )
      )
    write(thePlot)
  end lteBenchmark

  def matMulBenchmark =
    val thePlot = BenchmarkPlotElements.schema ++
      BenchmarkPlotElements.data("../../benchmarks/benchmark_history.json") ++
      (
        transform = BenchmarkPlotElements.transform(
          List("DgemmBenchmark.java_dgemm", "DgemmBenchmark.vecxt_mmult")
        )
      ) ++ BenchmarkPlotElements.matmulLayer

    write(thePlot)
  end matMulBenchmark

end BenchmarkPlots