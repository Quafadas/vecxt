package vecxt.plot

import scala.annotation.meta.field
import upickle.default.*
import NamedTupleReadWriter.given
import scala.collection.immutable.Stream.Empty

object BenchmarkPlotElements:
  final val schema: ($schema: String) = (`$schema` = "https://vega.github.io/schema/vega-lite/v5.json")

  val fakeData = (
    data = (
      values = Fake.fakeData,
      format = (
        `type` = "json"
      )
    )
  )

  def data(url: String): (data: (url: String, format: (`type`: String))) = (
    data = (
      url = url,
      format = (
        `type` = "json"
      )
    )
  )

  val calculate1 =
    (
      calculate = "replace(datum.benchmark, 'vecxt.benchmark.', '')",
      `as` = "benchmark"
    )
  val calculate2 =
    (
      calculate =
        "datetime(substring(datum.date,0, 4)+ '-' + substring(datum.date,4, 6) + '-' + substring(datum.date,6, 8))",
      `as` = "date"
    )

  val windowAndSortAndFilterLatest = Tuple2(
    (
      window = List(
        (
          op = "dense_rank",
          `as` = "rank"
        )
      ),
      sort = List(
        (
          field = "date",
          order = "descending"
        )
      )
    ),
    (
      filter = "datum.rank <= 1"
    )
  )

  val windowAndSort = Tuple(
    (
      window = List(
        (
          op = "dense_rank",
          `as` = "rank"
        )
      ),
      sort = List(
        (
          field = "date",
          order = "descending"
        )
      )
    )
  )

  def timeTransform(listBenchmarksToPlot: List[String]) = calculate1 *: calculate2 *: (
    filter = (
      field = "benchmark",
      oneOf = listBenchmarksToPlot
    )
  ) *: windowAndSort

  def transform(listBenchmarksToPlot: List[String]) = calculate1 *: calculate2 *:
    (
      joinaggregate = Tuple(
        (
          op = "max",
          field = "date",
          `as` = "maxDate"
        )
      )
    ) *:
    (
      filter = (
        field = "benchmark",
        oneOf = listBenchmarksToPlot
      )
    )
    *: windowAndSortAndFilterLatest

  def timeLayer(n: Int, benchmarkVariableName: String) = (
    layer = Tuple2(
      (
        title = s"n = $n",
        mark = (
          `type` = "line",
          color = "black"
        ),
        encoding = (
          y = (
            field = "score",
            `type` = "quantitative",
            scale = (
              zero = false
            ),
            title = "ops/s"
          ),
          x = (
            field = "date",
            timeUnit = "date"
          )
        )
      ),
      (
        mark = (
          opacity = 0.3,
          `type` = "area",
          color = "#85C5A6"
        ),
        encoding = (
          x = (
            field = "date",
            timeUnit = "date"
          ),
          y = (
            field = "scoreUpperConfidence",
            `type` = "quantitative"
          ),
          y2 = (
            field = "scoreLowerConfidence"
          )
        )
      )
    ),
    transform = List(
      (filter = s"(datum.params.$benchmarkVariableName == '$n')")
    )
  )

  def layer(n: Int, benchmarkVariableName: String, yZeroScale: Boolean = false) =
    (
      layer = Tuple2(
        (title = s"n = $n") ++ errorbar(yZeroScale),
        markPart
      ),
      transform = List(
        (filter = s"(datum.params.$benchmarkVariableName == '$n')")
      )
    )
  end layer

  def errorbar(yZeroScale: Boolean = false) =

    val yScale = (
      field = "benchmark",
      `type` = "ordinal",
      scale = (
        zero = yZeroScale
      )
    )

    (
      mark = "errorbar",
      encoding = (
        x = (
          field = "scoreLowerConfidence",
          `type` = "quantitative",
          scale = (
            zero = false
          ),
          title = "ops / s"
        ),
        y = yScale,
        x2 = (
          field = "scoreUpperConfidence"
        )
      )
    )
  end errorbar

  val markPart = (
    mark = (
      `type` = "point",
      filled = true,
      color = "black"
    ),
    encoding = (
      x = (
        field = "score",
        `type` = "quantitative"
      ),
      y = (
        field = "benchmark",
        `type` = "ordinal"
      )
    )
  )

  val matmulLayer = (
    layer = Tuple2(
      errorbar(yZeroScale = false),
      markPart
    )
  )

end BenchmarkPlotElements
