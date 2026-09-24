package vecxt_re

object PortfolioCalc:

  class Position(values: Map[String, Any]) extends Selectable:
    type Fields =
      (price: Price, notional: Double, xcRateToFundCurrency: Double, riskFreeRate: RiskFreeRate, spread: Spread)
    def selectDynamic(f: String): Any = values(f)
  end Position

  // def pnl1Year(
  //   holdings: IndexedSeq[Position],
  //   lossMatrix: Array[Double]
  // ) =
  //   val oneYearYield = for h <- holdings yield {
  //     val riskFreeInterest = riskFreeRateUnit.convert(1.0, PriceUnit.One) * riskFreeRate
  //     val spreadInOne = spreadUnit.convert(spread, PriceUnit.One)
  //     val priceInOne = priceUnit.convert(price, PriceUnit.One)
  //   }
end PortfolioCalc
