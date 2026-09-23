package vecxt_re

import vecxt.all.Matrix
import vecxt.dimensionExtender.DimensionExtender.Dimension
import vecxt.DoubleMatrix.reduceAlongDimension
import vecxt.all.mean
import vecxt.all.sum
import vecxt.all.>
import vecxt.all.trues

/** The assumption here.
  *
  * Potential future outcomes of a single transactions run down columns.
  *
  * Each row represents one potential future outcome, across a portfolio
  */
opaque type LossMatrix = Matrix[Double]

object LossMatrix:
  inline def apply(m: Matrix[Double]): LossMatrix = m
end LossMatrix

extension (m: LossMatrix)
  def transactionEls = m.mean(Dimension.Cols)

  def el = m.mean

  /** The probability of paying any loss
    *
    * @return
    */
  def attachProb = (m.sum(Dimension.Rows) > 0).trues.toDouble / m.rows.toDouble
end extension
