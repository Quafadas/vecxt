//> using scala 3.5.0

//> using dep com.lihaoyi::os-lib:0.10.7
//> using dep com.lihaoyi::upickle:4.0.1

import os._
import ujson._

@main def aggregate(): Unit = {
  val benchmarkCacheDir = os.pwd
  val outputFile = os.pwd / "benchmark_history.json"

  // Read all JSON files in the benchmark_cache directory
  val allJson =
    os.list(benchmarkCacheDir).filter(_.ext == "json").map { file =>
      println(file)
      val jsonString = os.read(file)
      ujson.read(jsonString)
    }

  // Write the combined JSON array to the output file
  val mainOnly = allJson.map { json =>
    json("branch").str == "main"
  }

  os.write(outputFile, ujson.write(allJson, indent = 4))

  println(s"Combined JSON written to ${outputFile}")
}
