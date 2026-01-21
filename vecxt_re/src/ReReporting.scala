package vecxt_re

import vecxt.all.*

object ReReporting: 
  extension(calcd: (layer: Layer, cededToLayer: Array[Double]))

    inline def attachmentProbability(numIterations: Int) = (calcd.cededToLayer > 0).trues / numIterations.toDouble

    inline def exhaustionProbability(numIterations: Int) = 
      val exhaust = calcd.layer.aggLimit.getOrElse(Double.PositiveInfinity) - 0.01
      (calcd.cededToLayer > exhaust).trues / numIterations.toDouble

    inline def expectedLoss(numIterations: Int) = calcd.cededToLayer.sum / numIterations

    inline def std(numIterations: Int, years: Array[Int]) = groupSum(years, calcd.cededToLayer, numIterations).stdDev
        
    inline def expectedLossAggLimit(numIterations: Int) = calcd.cededToLayer.sum / (calcd.layer.aggLimit.getOrElse(Double.PositiveInfinity) * numIterations)

    inline def lossReport(numIterations: Int, limit: ReportDenominator ) = 
        (
            reportLimit = limit(layer)
            attachmentProbability = attachmentProbability()
        )




