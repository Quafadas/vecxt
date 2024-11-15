/*
 *
 * Copyright 2023 quafadas
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package vecxt.plot

import upickle.default.*
import NamedTupleReadWriter.given
import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.*
import scalatags.JsDom.short.*

class PlotChecks extends munit.FunSuite:

  test("something") {

    val thePlot = BenchmarkPlotElements.schema ++
      BenchmarkPlotElements.data("../../benchmarks/benchmark_history.json") ++
      (transform =
        BenchmarkPlotElements.timeTransform(
          List("vecxt.benchmark.AddScalarBenchmark.vecxt_add_vec")
        )
      ) ++
      (vconcat =
        List(
          BenchmarkPlotElements.layer(3),
          BenchmarkPlotElements.layer(1000),
          BenchmarkPlotElements.layer(100000)
        )
      )

    val json = writeJs(thePlot)

    // println(json)

    // assert(json.toString().contains("$schema\":"))

  }

  test("over time plot") {
    val plot = BenchmarkPlots.addScalarBenchmarkOverTime

    println(plot)

    assert(plot.contains("data"))

  }

end PlotChecks
