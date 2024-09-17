//> using scala 3.5.0

//> using dep com.lihaoyi::os-lib:0.10.7
// //> using dep com.lihaoyi::upickle:4.0.1
//> using dep io.github.quafadas::dedav4s:0.9.3

import os._
import ujson._

import viz.vega.plots.BarChart
import viz.PlotTargets.desktopBrowser
import viz.extensions.jvm.plotFromFile

@main def plot_matmul: Unit = {
  val benchmarkCacheDir = os.pwd
  val inputFile = os.pwd / "benchmark_history.json"
  val jsonString = ujson.read(os.read(inputFile))
  // Filter out entries where branch is "main" and benchmark contains "mmult"
  val filteredData = jsonString.arr.filter { entry =>
    entry.obj("branch").str == "main" &&
    entry.obj("data").arr.exists(_.obj("benchmark").str.contains("mmult"))
  }

  // Select the entry with the largest "date" field
  val latestEntry =
    filteredData.maxBy(entry => entry.obj("date").str.toInt)

  val data =
    latestEntry.obj("data").arr.map( j =>
      j("lower_confidence") = j("primaryMetric").obj("scoreConfidence").arr.head.num
      j("upper_confidence") = j("primaryMetric").obj("scoreConfidence").arr.last.num
      j("score") = j("primaryMetric").obj("score").num
      j("benchmark") = j("benchmark").str.replace("vecxt.benchmark.", "")
      j
    )


  println(data)

  plotFromFile(
    os.pwd / "plot_templates" / "error_bar.json",
    List(
      (spec: ujson.Value) => spec("data") = ujson.Obj("values" -> data),
      viz.Utils.fillDiv
    )
  )

}
