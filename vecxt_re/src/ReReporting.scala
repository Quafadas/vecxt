package vecxt_re

import vecxt.all.*

object ReReporting:
  extension (calcd: (layer: Layer, cededToLayer: Array[Double]))

    inline def attachmentProbability(numIterations: Int, years: Array[Int]): Double =
      (groupSum(years, calcd.cededToLayer, numIterations) > 0).trues / numIterations.toDouble

    inline def exhaustionProbability(numIterations: Int, years: Array[Int]): Double =
      val exhaust = calcd.layer.aggLimit.getOrElse(Double.PositiveInfinity) - 0.01
      (groupSum(years, calcd.cededToLayer, numIterations) > exhaust).trues / numIterations.toDouble
    end exhaustionProbability

    inline def expectedLoss(numIterations: Int): Double = calcd.cededToLayer.sum / numIterations

    inline def std(numIterations: Int, years: Array[Int]): Double =
      groupSum(years, calcd.cededToLayer, numIterations).stdDev

    inline def expectedLossAggLimit(numIterations: Int): Double =
      calcd.cededToLayer.sum / (calcd.layer.aggLimit.getOrElse(Double.PositiveInfinity) * numIterations)

    inline def lossReport(
        numIterations: Int,
        years: Array[Int],
        limit: ReportDenominator
    ): (name: String, limit: Double, el: Double, stdDev: Double, attachProb: Double, exhaustProb: Double) =
      val reportLimit = limit.fromlayer(calcd.layer)
      (
        name = calcd.layer.layerName.getOrElse(s"Layer ${calcd.layer.layerId}"),
        limit = reportLimit,
        el = expectedLoss(numIterations) / reportLimit,
        stdDev = std(numIterations, years) / reportLimit,
        attachProb = attachmentProbability(numIterations, years),
        exhaustProb = exhaustionProbability(numIterations, years)
      )

      // TODO formatting
  end extension
end ReReporting
