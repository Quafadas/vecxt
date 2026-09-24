package vecxt_re

enum Ccy derives CanEqual:
  case USD, EUR, GBP, CHF, JPY, AUD, CAD, NZD

/** The scale a monetary amount is quoted in. `multiplier` is the number of major currency units in one of this unit. */
enum AmountUnit(val multiplier: Long):
  case One extends AmountUnit(1L)
  case K extends AmountUnit(1_000L)
  case Mn extends AmountUnit(1_000_000L)
  case Bn extends AmountUnit(1_000_000_000L)
end AmountUnit

/** 
 *  This should _NOT_ be used for actual cash values. We use double here 
 *  because this intended for financial simulation where errors of <0.01 are meaningless.
  * An absolute monetary quantity denominated in a single currency,
  * e.g. a notional, limit, premium or loss.
  *
  * Amounts in different currencies are never implicitly combined:
  * arithmetic between currencies fails, and conversion requires an
  * explicit FX rate. Scaling by a [[Rel]] yields a CurrencyAmount
  * (premium = notional × coupon); dividing two amounts in the same
  * currency yields a [[Rel]].
  *
  * Stored as a Double in major currency units; [[AmountUnit]] only affects construction and display.
  */
final case class CurrencyAmount private (amount: Double, ccy: Ccy) derives CanEqual:

  /** The amount expressed in unit `u`, e.g. `CurrencyAmount(2.5, Mn, USD).in(K) == 2500.0` */
  def in(u: AmountUnit): Double = amount / u.multiplier

  def +(y: CurrencyAmount): CurrencyAmount =
    sameCcy(y); new CurrencyAmount(amount + y.amount, ccy)

  def -(y: CurrencyAmount): CurrencyAmount =
    sameCcy(y); new CurrencyAmount(amount - y.amount, ccy)

  def unary_- : CurrencyAmount = new CurrencyAmount(-amount, ccy)

  /** premium = notional * coupon */
  def *(r: Rel): CurrencyAmount =
    new CurrencyAmount(amount * r.canonical, ccy)

  def *(k: Double): CurrencyAmount =
    new CurrencyAmount(amount * k, ccy)

  /** Ratio of two same-currency amounts, e.g. a loss ratio. */
  def /(y: CurrencyAmount): Rel =
    sameCcy(y); Rel(amount / y.amount, PriceUnit.One)

  def approxEq(y: CurrencyAmount, tol: Double = 1e-6): Boolean =
    sameCcy(y); math.abs(amount - y.amount) <= tol

  override def toString: String = f"$amount%.2f $ccy"

  private def sameCcy(y: CurrencyAmount): Unit =
    require(ccy == y.ccy, s"Currency mismatch: $ccy vs ${y.ccy}")

object CurrencyAmount:
  /** e.g. `CurrencyAmount(2.5, AmountUnit.Mn, Ccy.USD)` is 2,500,000 USD */
  def apply(amount: Double, unit: AmountUnit, ccy: Ccy): CurrencyAmount =
    new CurrencyAmount(amount * unit.multiplier, ccy)

  def zero(ccy: Ccy): CurrencyAmount = new CurrencyAmount(0.0, ccy)

  given Ordering[CurrencyAmount] with
    def compare(a: CurrencyAmount, b: CurrencyAmount): Int =
      require(a.ccy == b.ccy, s"Cannot order ${a.ccy} vs ${b.ccy}")
      java.lang.Double.compare(a.amount, b.amount)