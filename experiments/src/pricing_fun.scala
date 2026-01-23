package experiments

import RPT.*
import cats.syntax.all.*
import io.github.quafadas.table.TypeInferrer
import vecxt.BoundsCheck.DoBoundsCheck.yes

@main def pricingFun =

  val data = CSV.resource("losses.csv", CsvOpts(TypeInferrer.FromAllRows, ReadAs.Columns))

  val scen = Scenarr(
    iterations = data.year,
    days = data.day,
    amounts = data.amount,
    numberIterations = 10,
    threshold = 0.0
  ).sorted

  // val scen1 = scen.iteration(1).copy(numberIterations = 1)

  // println(scen1)
  scen.itrDayAmount.ptbln

  val tower = Tower.singleShot(500e6, Array(150e6, 150e6, 100e6))
  val tower2 = Tower.singleShot(900e6, Array(100e6))

  val (ceded, retained, splits) = tower.splitScenarioAmounts(scen)
  val (ceded2, retained2, splits2) = tower2.splitScenarioAmounts(scen)

  // println(ceded.printArr)
  // println(retained.printArr)

  // println()

  (splits ++ splits2).map(s => s.lossReport(scen.numberIterations, scen.iterations, ReportDenominator.FirstLimit)).ptbln

  // val (ceded10, retained10, splits10) = tower.splitScenarioAmounts(iter10)(using true)

  // println(ceded10.printArr)

  // splits10.map(_.cededToLayer).foreach(arr => println(arr.printArr))

  // splits.map(s => s.lossReport(scen.numberIterations, scen.iterations, ReportDenominator.FirstLimit)).ptbln
end pricingFun
