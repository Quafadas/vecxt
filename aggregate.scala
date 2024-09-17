//> using scala 3.5.0

//> using dep com.lihaoyi:os-lib:0.10.2
//> using dep com.lihaoyi:upickle:4.0.0

import os._
import ujson._

@main def aggregate(): Unit = {
  def main(args: Array[String]): Unit = {
    val benchmarkCacheDir = os.pwd / "benchmark_cache"
    val outputFile = os.pwd / "benchmark_history.json"

    // Create a list to hold all JSON objects
    val jsonObjects = scala.collection.mutable.ListBuffer[Value]()

    // Read all JSON files in the benchmark_cache directory
    if (os.exists(benchmarkCacheDir) && os.isDir(benchmarkCacheDir)) {
      os.list(benchmarkCacheDir).filter(_.ext == "json").foreach { file =>
        val jsonString = os.read(file)
        val json = ujson.read(jsonString)
        jsonObjects += json
      }
    }

    // Write the combined JSON array to the output file
    val combinedJson = ujson.Arr(jsonObjects: _*)
    os.write(outputFile, ujson.write(combinedJson, indent = 4))

    println(s"Combined JSON written to ${outputFile}")
  }
}
