package vecxt_re

import vecxt.all.*

object ReReporting: 
  extension(calcd: (layer: Layer, cededToLayer: Array[Double]))

    inline def attachmentProbability(numIterations: Int): Double = (calcd.cededToLayer > 0).trues / numIterations.toDouble

    inline def exhaustionProbability(numIterations: Int, years: Array[Int]): Double = 
      val exhaust = calcd.layer.aggLimit.getOrElse(Double.PositiveInfinity) - 0.01
      (groupSum(years, calcd.cededToLayer, numIterations) > exhaust).trues / numIterations.toDouble

    inline def expectedLoss(numIterations: Int): Double = calcd.cededToLayer.sum / numIterations

    inline def std(numIterations: Int, years: Array[Int]): Double = groupSum(years, calcd.cededToLayer, numIterations).stdDev
        
    inline def expectedLossAggLimit(numIterations: Int): Double = calcd.cededToLayer.sum / (calcd.layer.aggLimit.getOrElse(Double.PositiveInfinity) * numIterations)

    inline def lossReport(numIterations: Int, years: Array[Int], limit: ReportDenominator ) : (limit: Double, el: Double, stdDev: Double, attachProb: Double, exhaustProb: Double) = 
      (
        limit = limit.fromlayer(calcd.layer),
        el = expectedLoss(numIterations),
        stdDev = std(numIterations, years),
        attachProb = attachmentProbability(numIterations),
        exhaustProb = exhaustionProbability(numIterations, years)
      )

      //TODO formatting




