package vecxt_re

import java.time.LocalDate

import scala.NamedTuple.NamedTuple

import vecxt.all.*

/** Portfolio level calculations over a collection of positions.
  *
  * Positions are record-like views over a `Map[String, Any]`, exposing typed fields via [[scala.Selectable]]. Callers
  * need not construct them directly: the public entry points accept any named tuple carrying the required fields (see
  * [[NamedRecord]]).
  */
object PortfolioCalc:

  /** A holding for forward-looking (e.g. P&L) calculations.
    *
    * @note
    *   Fields are currently identical to [[PositionT0]]; kept separate as the two are expected to diverge.
    */
  class PositionT1(values: Map[String, Any]) extends Selectable:
    /** @param price
      *   the market price, as a [[Price]] relative to par
      * @param notional
      *   the face amount, carrying its own currency
      * @param name
      *   a human readable label, used in error messages
      * @param id
      *   a unique identifier, used in error messages
      */
    type Fields =
      (
          price: Price,
          riskFree: RiskFreeRate,
          spread: Spread,
          maturity: LocalDate,
          notional: CurrencyAmount,
          name: String,
          id: String
      )
    def selectDynamic(f: String): Any = values(f)
  end PositionT1

  /** A holding as observed at the valuation date (day 0).
    *
    * Normally built from a named tuple by [[day0MarketValues]]; constructing one directly from a `Map` is unchecked,
    * and a missing key or a wrongly typed value only surfaces when the field is accessed.
    */
  class PositionT0(values: Map[String, Any]) extends Selectable:
    /** @param price
      *   the market price, as a [[Price]] relative to par
      * @param notional
      *   the face amount, carrying its own currency
      * @param name
      *   a human readable label, used in error messages
      * @param id
      *   a unique identifier, used in error messages
      */
    type Fields =
      (price: Price, notional: CurrencyAmount, name: String, id: String)
    def selectDynamic(f: String): Any = values(f)
  end PositionT0

  /** The day 0 market value of each position, in the portfolio currency.
    *
    * For each position the value is `xcRate(notional.ccy) * notional * price`, where the price is taken in canonical
    * units (so `Price(102, Pts)` contributes a factor of 1.02).
    *
    * Positions may be supplied as any named tuple holding the fields of [[PositionT0.Fields]], in any order; extra
    * fields are ignored. A missing or mistyped field is a compile error. All elements of the list must share one field
    * order, as the list has a single element type.
    *
    * {{{
    * day0MarketValues(
    *   List((id = "1", name = "Bond A", price = Price(102, PriceUnit.Pts), notional = CurrencyAmount(1, Mn, USD))),
    *   portfolioCurrency = Ccy.CHF,
    *   xcRates = Map(Ccy.USD -> 0.9)
    * ) // Array(918000.0)
    * }}}
    *
    * @param positions
    *   the holdings to value
    * @param portfolioCurrency
    *   the currency results are expressed in. Positions already in this currency use a rate of 1.0 and need no entry in
    *   `xcRates`.
    * @param xcRates
    *   units of `portfolioCurrency` per one unit of the keyed currency. Entries for currencies no position uses are
    *   ignored.
    * @return
    *   one value per position, in input order
    * @throws IllegalArgumentException
    *   if any position's currency has no FX rate. All such positions are reported together, one per line, each with its
    *   id and name.
    */
  inline def day0MarketValues[N <: Tuple, V <: Tuple](
      positions: List[NamedTuple[N, V]],
      portfolioCurrency: Ccy,
      xcRates: Map[Ccy, Double]
  ): Array[Double] =
    NamedRecord.check[N, V, PositionT0#Fields]
    val names = NamedRecord.names[N]
    day0MarketValuesOf(positions.map(p => PositionT0(NamedRecord.toMap(names, p))), portfolioCurrency, xcRates)
  end day0MarketValues

  /** As [[day0MarketValues]], for positions already built as [[PositionT0]].
    *
    * @throws IllegalArgumentException
    *   if any position's currency has no FX rate; see [[day0MarketValues]]
    */
  def day0MarketValuesOf(
      positions: List[PositionT0],
      portfolioCurrency: Ccy,
      xcRates: Map[Ccy, Double]
  ): Array[Double] =
    val results: List[Either[String, Double]] = positions.map { p =>
      val ccy = p.notional.ccy
      val xcRate = if ccy == portfolioCurrency then Some(1.0) else xcRates.get(ccy)
      xcRate
        .map(_ * (p.notional * p.price).amount)
        .toRight(
          s"Position id=${p.id} name=${p.name}: no FX rate from $ccy to $portfolioCurrency"
        )
    }
    val errors = results.collect { case Left(e) => e }
    if errors.nonEmpty then
      throw new IllegalArgumentException(
        s"Failed to compute day 0 market values for ${errors.size} position(s):\n${errors.mkString("\n")}"
      )
    end if
    results.collect { case Right(v) => v }.toArray
  end day0MarketValuesOf

  def marketValueForecast1Year(
      holdings: IndexedSeq[PositionT1],
      lossMatrix: Matrix[Double],
      t0Date: LocalDate,
      xcRates: Map[Ccy, Double],
      portfolioCurrency: Ccy
  ): Matrix[Double] =
    require(
      holdings.length == lossMatrix.cols,
      s"Each entry in the lossMatrix should have a holdings entry. Loss matrix width ${lossMatrix.cols}, got ${holdings.length} holdings"
    )
    val oneYearYield = for h <- holdings yield
      val priceT1 = PositionCalculations.priceForecast1Year(h.price, t0Date, h.maturity)
      val ccy = h.notional.ccy
      val xcRate =
        if ccy == portfolioCurrency then Right[String, Double](1.0)
        else
          xcRates
            .get(ccy)
            .toRight(
              s"Position id=${h.id} name=${h.name}: no FX rate from $ccy to $portfolioCurrency"
            )

      xcRate.map { xc =>
        val notionalInPortfolioCurrency = xc * h.notional.amount
        ((priceT1 + h.riskFree + h.spread).canonical, notionalInPortfolioCurrency)
      }

    val errors = oneYearYield.collect { case Left(e) => e }
    if errors.nonEmpty then
      throw new IllegalArgumentException(
        s"Failed to compute 1 year market value forecast for ${errors.size} position(s):\n${errors.mkString("\n")}"
      )
    end if

    val (unitValueT1, notionals) = oneYearYield.collect { case Right(r) => r }.unzip
    val n = notionals.toArray

    // MV(i, j) = n(j) * (unitValueT1(j) - loss(i, j)) = loss(i, j) * -n(j) + unitValueT1(j) * n(j), in one pass
    lossMatrix.fmaCols(multiply = n * -1.0, add = unitValueT1.toArray * n)
  end marketValueForecast1Year

  /** The one year relative return of each position in each loss scenario, in canonical units (`0.05` is 5%).
    *
    * The return is measured against the day 0 market value `notional * price`:
    *
    * `r(i, j) = (priceT1(j) + riskFree(j) + spread(j) - loss(i, j)) / price(j) - 1`
    *
    * i.e. [[marketValueForecast1Year]] divided by the day 0 market value, minus one. Notional and FX cancel (both are
    * taken at the day 0 rate), so no FX rates are needed.
    *
    * @param lossMatrix
    *   one row per scenario, one column per holding; positive losses in canonical units of notional
    * @return
    *   a `lossMatrix.rows x holdings.length` matrix of canonical relative returns
    * @throws IllegalArgumentException
    *   if the column count does not match the holdings, or any holding has a non-positive price (all such holdings are
    *   reported together, one per line)
    */
  def relativeReturn1Year(
      holdings: IndexedSeq[PositionT1],
      lossMatrix: Matrix[Double],
      t0Date: LocalDate
  ): Matrix[Double] =
    require(
      holdings.length == lossMatrix.cols,
      s"Each entry in the lossMatrix should have a holdings entry. Loss matrix width ${lossMatrix.cols}, got ${holdings.length} holdings"
    )
    val perHolding = holdings.map { h =>
      val price0 = h.price.canonical
      if price0 > 0.0 then
        val unitValueT1 =
          (PositionCalculations.priceForecast1Year(h.price, t0Date, h.maturity) + h.riskFree + h.spread).canonical
        Right((-1.0 / price0, unitValueT1 / price0 - 1.0))
      else Left(s"Position id=${h.id} name=${h.name}: price ${h.price} must be positive to compute a relative return")
      end if
    }
    val errors = perHolding.collect { case Left(e) => e }
    if errors.nonEmpty then
      throw new IllegalArgumentException(
        s"Failed to compute 1 year relative return for ${errors.size} position(s):\n${errors.mkString("\n")}"
      )
    end if

    val (multiply, add) = perHolding.collect { case Right(r) => r }.unzip
    // r(i, j) = (c(j) - loss(i, j)) / p0(j) - 1 = loss(i, j) * (-1 / p0(j)) + (c(j) / p0(j) - 1), in one pass
    lossMatrix.fmaCols(multiply.toArray, add.toArray)
  end relativeReturn1Year

end PortfolioCalc
