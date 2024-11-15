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

import scalajs.js
import upickle.default.*
import NamedTupleReadWriter.given
import scalatags.JsDom.short.*
import scala.scalajs.js.annotation.JSImport
import vecxt.plot.BenchmarkPlotElements.fakeData

@js.native
@JSImport("fs", JSImport.Namespace)
object FS extends js.Object:
  def readFileSync(path: String, encoding: String, callback: js.Function2[js.Error, String, Unit]): String =
    js.native
end FS

@js.native
@JSImport("process", JSImport.Namespace)
object Process extends js.Object:
  def cwd(): String = js.native
end Process

class PlotChecks extends munit.FunSuite:

  // test("some test data") {
  //   val argy = Fake.fakeData.arr.filter(d => d("benchmark").str.contains("AddScalarBenchmark.vecxt_add_vec"))
  //   println(argy)

  // }

  test("Add scalar plot") {
    assertEquals(
      BenchmarkPlots.addScalarBenchmark,
      """{"$schema":"https://vega.github.io/schema/vega-lite/v5.json","data":{"url":"../../benchmarks/benchmark_history.json","format":{"type":"json"}},"transform":[{"calculate":"replace(datum.benchmark, 'vecxt.benchmark.', '')","as":"benchmark"},{"calculate":"datetime(substring(datum.date,0, 4)+ '-' + substring(datum.date,4, 6) + '-' + substring(datum.date,6, 8))","as":"date"},{"joinaggregate":[{"op":"max","field":"date","as":"maxDate"}]},{"filter":{"field":"benchmark","oneOf":["AddScalarBenchmark.vecxt_add","AddScalarBenchmark.vecxt_add_vec"]}},{"window":[{"op":"dense_rank","as":"rank"}],"sort":[{"field":"date","order":"descending"}]},{"filter":"datum.rank <= 1"}],"vconcat":[{"layer":[{"title":"n = 10","mark":"errorbar","encoding":{"x":{"field":"scoreLowerConfidence","type":"quantitative","scale":{"zero":false},"title":"ops / s"},"y":{"field":"benchmark","type":"ordinal","scale":{"zero":false}},"x2":{"field":"scoreUpperConfidence"}}},{"mark":{"type":"point","filled":true,"color":"black"},"encoding":{"x":{"field":"score","type":"quantitative"},"y":{"field":"benchmark","type":"ordinal"}}}],"transform":[{"filter":"(datum.params.n == '10')"}]},{"layer":[{"title":"n = 1000","mark":"errorbar","encoding":{"x":{"field":"scoreLowerConfidence","type":"quantitative","scale":{"zero":false},"title":"ops / s"},"y":{"field":"benchmark","type":"ordinal","scale":{"zero":false}},"x2":{"field":"scoreUpperConfidence"}}},{"mark":{"type":"point","filled":true,"color":"black"},"encoding":{"x":{"field":"score","type":"quantitative"},"y":{"field":"benchmark","type":"ordinal"}}}],"transform":[{"filter":"(datum.params.n == '1000')"}]},{"layer":[{"title":"n = 100000","mark":"errorbar","encoding":{"x":{"field":"scoreLowerConfidence","type":"quantitative","scale":{"zero":false},"title":"ops / s"},"y":{"field":"benchmark","type":"ordinal","scale":{"zero":false}},"x2":{"field":"scoreUpperConfidence"}}},{"mark":{"type":"point","filled":true,"color":"black"},"encoding":{"x":{"field":"score","type":"quantitative"},"y":{"field":"benchmark","type":"ordinal"}}}],"transform":[{"filter":"(datum.params.n == '100000')"}]}]}"""
    )
  }

  test("Add scalar plot over time plot") {

    assertEquals(
      BenchmarkPlots.addScalarBenchmarkOverTime,
      """{"$schema":"https://vega.github.io/schema/vega-lite/v5.json","data":{"url":"../../benchmarks/benchmark_history.json","format":{"type":"json"}},"transform":[{"calculate":"replace(datum.benchmark, 'vecxt.benchmark.', '')","as":"benchmark"},{"calculate":"datetime(substring(datum.date,0, 4)+ '-' + substring(datum.date,4, 6) + '-' + substring(datum.date,6, 8))","as":"date"},{"filter":{"field":"benchmark","oneOf":["AddScalarBenchmark.vecxt_add_vec"]}},{"window":[{"op":"dense_rank","as":"rank"}],"sort":[{"field":"date","order":"descending"}]}],"vconcat":[{"layer":[{"title":"n = 3","mark":{"type":"line","color":"black"},"encoding":{"y":{"field":"score","type":"quantitative","scale":{"zero":false},"title":"ops/s"},"x":{"field":"date","timeUnit":"date"}}},{"mark":{"opacity":0.3,"type":"area","color":"#85C5A6"},"encoding":{"x":{"field":"date","timeUnit":"date"},"y":{"field":"scoreUpperConfidence","type":"quantitative"},"y2":{"field":"scoreLowerConfidence"}}}],"transform":[{"filter":"(datum.params.n == '10')"}]},{"layer":[{"title":"n = 1000","mark":{"type":"line","color":"black"},"encoding":{"y":{"field":"score","type":"quantitative","scale":{"zero":false}},"x":{"field":"date","timeUnit":"date"}}},{"mark":{"opacity":0.3,"type":"area","color":"#85C5A6"},"encoding":{"x":{"field":"date","timeUnit":"date"},"y":{"field":"scoreUpperConfidence","type":"quantitative","title":"ops/s"},"y2":{"field":"scoreLowerConfidence"}}}],"transform":[{"filter":"(datum.params.n == '1000')"}]},{"layer":[{"title":"n = 1000000","mark":{"type":"line","color":"black"},"encoding":{"y":{"field":"score","type":"quantitative","scale":{"zero":false},"title":"ops/s"},"x":{"field":"date","timeUnit":"date"}}},{"mark":{"opacity":0.3,"type":"area","color":"#85C5A6"},"encoding":{"x":{"field":"date","timeUnit":"date"},"y":{"field":"scoreUpperConfidence","type":"quantitative"},"y2":{"field":"scoreLowerConfidence"}}}],"transform":[{"filter":"(datum.params.n == '100000')"}]}]}"""
    )

  }

end PlotChecks
