package experiments

import RPT.*
import cats.syntax.all.*

@main def pricingFun =

  val iterations = Array(1, 1, 2, 3, 1, 2, 3, 4, 5, 10, 10, 10, 10, 10).sorted
  val days = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
  val amounts = Array(20.0, 0, 0, 0, Int.MaxValue, 0, 0, 0, 0, 0, 25, 30, 0, 0)

  println(iterations.printArr)
  println(amounts.printArr)

  val scen = Scenarr(
    iterations = iterations,
    days = days,
    amounts = amounts,
    numberIterations = 10,
    threshold = 0.0
  )

  val tower = Tower.singleShot(15, Array(10, 10, 10))

  val iter10 = scen.iteration(10)

  val (ceded, retained, splits) = tower.splitScenarioAmounts(scen)(using true)
  val (ceded10, retained10, splits10) = tower.splitScenarioAmounts(iter10)(using true)

  println(ceded10.printArr)

  splits10.map(_.cededToLayer).foreach(arr => println(arr.printArr))

  splits.map(s => s.lossReport(scen.numberIterations, scen.iterations, ReportDenominator.FirstLimit)).ptbln
end pricingFun
