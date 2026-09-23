package vecxt_re

import java.time.LocalDate
import java.time.temporal.ChronoUnit
import vecxt.all.-

object PositionCalculations:
  /** Forecasts the price of a bond which is being pulled to parity assuming a one year time horizon.
    *
    * This allows us to ignore seasonality, which is assumed to be annual.
    *
    * @param price
    *   the current price, expressed in `priceUnit`
    * @param priceUnit
    *   the unit `price` is quoted in (e.g. `Pts` for a price of 102 meaning 102% of par)
    * @param priceDate
    *   the date on which `price` was observed; the forecast is for one year after this date
    * @param maturity
    *   the maturity date of the bond
    * @return
    *   the forecast price one year after `priceDate`, in `priceUnit`. If the bond matures within the year, this is par.
    */
  def priceForecast1Year(
      price: Double,
      priceUnit: PriceUnit,
      priceDate: LocalDate,
      maturity: LocalDate
  ): Double =
    price + pull2Parity1Year(price, priceUnit, priceDate, maturity)

  /** The change in price over one year of a bond which is being pulled linearly (by calendar days) to parity at
    * maturity.
    *
    * This allows us to ignore seasonality, which is assumed to be annual.
    *
    * Uses calendar days, not yield accretion.
    *
    * @param price
    *   the current price, expressed in `priceUnit`
    * @param priceUnit
    *   the unit `price` is quoted in
    * @param priceDate
    *   the date on which `price` was observed; the projection is for one year after this date
    * @param maturity
    *   the maturity date of the bond
    * @return
    *   the forecast price change (forecast price minus `price`) in `priceUnit`. Positive for a bond priced below par,
    *   negative above par. If the bond matures within the year, this is the full distance to par.
    */
  def pull2Parity1Year(
      price: Double,
      priceUnit: PriceUnit,
      priceDate: LocalDate,
      maturity: LocalDate
  ): Double =
    import scala.math.Ordered.orderingToOrdered
    assert(
      maturity >= priceDate,
      s"Maturity $maturity and pricing date $priceDate suggest this bond has already matured. It should not pull to parity"
    )

    val one = PriceUnit.One.convert(1.0, priceUnit)
    val projectionDate = priceDate.plusYears(1L)

    val days2Maturity = ChronoUnit.DAYS.between(priceDate, maturity)
    val days2Project = ChronoUnit.DAYS.between(priceDate, projectionDate)
    if maturity < projectionDate then one - price
    else days2Project.toDouble / days2Maturity.toDouble * (one - price)
    end if
  end pull2Parity1Year

  /** Investment is purchased on the price date
    * Investment is sold in one years time, it is assumed the market is at a steady state over this period with no material change in risk premia.
    * Annual seasonality - i.e. seasonality can be ignored over a single annual time period.
    * A deep liquid market which is not affected by purchase / sale
    * Investments renew at expiry into an equivalent bond at par, at the same risk spread and expected loss.
    * All capital is ultimately returned to investor - impaired bonds would not pull to parity
    * The lossVector is _assumed_ to be in units of `One`
    *
    * @param price the current price, expressed in `priceUnit`
    * @param priceUnit the unit `price` is quoted in
    * @param priceDate the date on which `price` was observed (the purchase date)
    * @param maturity the maturity date of the bond
    * @param lossVector - By convention all positive numbers
    * @param riskFreeRate the annual risk free rate, expressed in `riskFreeRateUnit` (e.g. 325 in `Bps` is 3.25%)
    * @param riskFreeRateUnit the unit `riskFreeRate` is quoted in
    * @return one element per entry of `lossVector`: the one year PnL in units of `One`, i.e. risk free interest
    *   plus pull to parity minus that loss
    */
  def pnlForecast1Year(
      price: Double,
      priceUnit: PriceUnit,
      priceDate: LocalDate,
      maturity: LocalDate,
      lossVector: Array[Double],
      riskFreeRate: Double,
      riskFreeRateUnit: PriceUnit,
      spread: Double,
      spreadUnit: PriceUnit
  ): Array[Double] =
    val riskFreeInterest = riskFreeRateUnit.convert(1.0, PriceUnit.One) * riskFreeRate
    val spreadInOne = spreadUnit.convert(spread, PriceUnit.One)
    val priceInOne = priceUnit.convert(price, PriceUnit.One)
    (spreadInOne + riskFreeInterest + pull2Parity1Year(priceInOne, PriceUnit.One, priceDate, maturity)) - lossVector
  end pnlForecast1Year
end PositionCalculations
